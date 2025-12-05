(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

open Opentelemetry_client
open Common_
module OTEL = Opentelemetry
module Config = Config

let get_headers = Config.Env.get_headers

let set_headers = Config.Env.set_headers

let n_errors = Atomic.make 0

let n_dropped = Atomic.make 0

(** Something to be sent via HTTP *)
module To_send = struct
  open Opentelemetry.Proto

  type t =
    | Send_metric of Metrics.resource_metrics list
    | Send_trace of Trace.resource_spans list
    | Send_logs of Logs.resource_logs list
end

(** start a thread in the background, running [f()] *)
let start_bg_thread (f : unit -> unit) : Thread.t =
  let unix_run () =
    let signals =
      [
        Sys.sigusr1;
        Sys.sigusr2;
        Sys.sigterm;
        Sys.sigpipe;
        Sys.sigalrm;
        Sys.sigstop;
      ]
    in
    ignore (Thread.sigmask Unix.SIG_BLOCK signals : _ list);
    f ()
  in
  (* no signals on Windows *)
  let run () =
    if Sys.win32 then
      f ()
    else
      unix_run ()
  in
  Thread.create run ()

let str_to_hex (s : string) : string =
  Opentelemetry_util.Util_bytes_.bytes_to_hex (Bytes.unsafe_of_string s)

module Exporter_impl : sig
  val n_bytes_sent : int Atomic.t

  class type t = object
    inherit OTEL.Exporter.t

    method shutdown : on_done:(unit -> unit) -> unit -> unit
  end

  val create : stop:bool Atomic.t -> config:Config.t -> unit -> t

  val shutdown : t -> on_done:(unit -> unit) -> unit
end = struct
  open Opentelemetry.Proto

  let n_bytes_sent : int Atomic.t = Atomic.make 0

  class type t = object
    inherit OTEL.Exporter.t

    method shutdown : on_done:(unit -> unit) -> unit -> unit
  end

  type state = {
    stop: bool Atomic.t;
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    config: Config.t;
    encoder_pool: Pbrt.Encoder.t Rpool.t;
    send_q: To_send.t Sync_queue.t;  (** Queue for the send worker threads *)
    traces: Proto.Trace.span Batch.t;
    logs: Proto.Logs.log_record Batch.t;
    metrics: Proto.Metrics.metric Batch.t;
    mutable send_threads: Thread.t array;  (** Threads that send data via http *)
  }

  let send_batch_ (self : state) ~force ~mk_to_send (b : _ Batch.t) : unit =
    match Batch.pop_if_ready ~force ~now:(Mtime_clock.now ()) b with
    | None -> ()
    | Some l ->
      let to_send = mk_to_send l in
      Sync_queue.push self.send_q to_send

  let send_http_ ~stop ~(config : Config.t) (client : Curl.t) ~url data : unit =
    let@ _sc =
      Self_trace.with_ ~kind:Span_kind_producer "otel-ocurl.send-http"
    in

    if Config.Env.get_debug () then
      Printf.eprintf "opentelemetry: send http POST to %s (%dB)\n%!" url
        (String.length data);
    let headers =
      ("Content-Type", "application/x-protobuf") :: config.common.headers
    in
    match
      let@ _sc =
        Self_trace.with_ ~kind:Span_kind_internal "curl.post"
          ~attrs:[ "sz", `Int (String.length data); "url", `String url ]
      in
      Ezcurl.post ~headers ~client ~params:[] ~url ~content:(`String data) ()
    with
    | Ok { code; _ } when code >= 200 && code < 300 ->
      if Config.Env.get_debug () then
        Printf.eprintf "opentelemetry: got response code=%d\n%!" code
    | Ok { code; body; headers = _; info = _ } ->
      Atomic.incr n_errors;
      Self_trace.add_event _sc
      @@ Opentelemetry.Event.make "error" ~attrs:[ "code", `Int code ];

      if Config.Env.get_debug () then (
        let dec = Pbrt.Decoder.of_string body in
        let body =
          try
            let status = Status.decode_pb_status dec in
            Format.asprintf "%a" Status.pp_status status
          with _ ->
            spf "(could not decode status)\nraw bytes: %s" (str_to_hex body)
        in
        Printf.eprintf
          "opentelemetry: error while sending data to %s:\n  code=%d\n  %s\n%!"
          url code body
      );
      ()
    | exception Sys.Break ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      Atomic.set stop true
    | Error (code, msg) ->
      (* TODO: log error _via_ otel? *)
      Atomic.incr n_errors;

      Printf.eprintf
        "opentelemetry: export failed:\n  %s\n  curl code: %s\n  url: %s\n%!"
        msg (Curl.strerror code) url;

      (* avoid crazy error loop *)
      Thread.delay 3.

  (** Thread that, in a loop, reads from [q] to get the next message to send via
      http *)
  let bg_thread_loop (self : state) : unit =
    Ezcurl.with_client ?set_opts:None @@ fun client ->
    let config = self.config in
    let stop = self.stop in
    let send ~name ~url ~conv (signals : _ list) =
      let@ _sp =
        Self_trace.with_ ~kind:Span_kind_producer name
          ~attrs:[ "n", `Int (List.length signals) ]
      in
      let msg : string =
        (* borrow encoder from buffer pool and turn [signals] into bytes *)
        let@ encoder = Rpool.with_resource self.encoder_pool in
        conv ?encoder:(Some encoder) signals
      in
      ignore (Atomic.fetch_and_add n_bytes_sent (String.length msg) : int);
      send_http_ ~stop ~config ~url client msg
    in
    try
      while not (Atomic.get stop) do
        let msg = Sync_queue.pop self.send_q in
        match msg with
        | To_send.Send_trace tr ->
          send ~name:"send-traces" ~conv:Signal.Encode.traces
            ~url:config.common.url_traces tr
        | To_send.Send_metric ms ->
          send ~name:"send-metrics" ~conv:Signal.Encode.metrics
            ~url:config.common.url_metrics ms
        | To_send.Send_logs logs ->
          send ~name:"send-logs" ~conv:Signal.Encode.logs
            ~url:config.common.url_logs logs
      done
    with Sync_queue.Closed -> ()

  let batch_max_size_ = 200

  let batch_timeout_ = Mtime.Span.(20 * s)

  let create_state ~stop ~config () : state =
    let n_send_threads = max 2 config.Config.bg_threads in
    let encoder_pool =
      Rpool.create
        ~mk_item:(fun () -> Pbrt.Encoder.create ~size:1024 ())
        ~max_size:32 ~clear:Pbrt.Encoder.reset ()
    in

    let self =
      {
        stop;
        config;
        send_threads = [||];
        send_q = Sync_queue.create ();
        encoder_pool;
        cleaned = Atomic.make false;
        traces = Batch.make ~batch:batch_max_size_ ~timeout:batch_timeout_ ();
        logs = Batch.make ~batch:batch_max_size_ ~timeout:batch_timeout_ ();
        metrics = Batch.make ~batch:batch_max_size_ ~timeout:batch_timeout_ ();
      }
    in

    self.send_threads <-
      Array.init n_send_threads (fun _i ->
          start_bg_thread (fun () -> bg_thread_loop self));

    self

  let maybe_send_metrics ~force (self : state) =
    send_batch_ self ~force self.metrics ~mk_to_send:(fun metrics ->
        let metrics =
          Opentelemetry_client.Util_resources.make_resource_metrics metrics
        in
        To_send.Send_metric [ metrics ])

  let maybe_send_logs ~force (self : state) =
    send_batch_ self ~force self.logs ~mk_to_send:(fun logs ->
        let logs =
          Opentelemetry_client.Util_resources.make_resource_logs logs
        in
        To_send.Send_logs [ logs ])

  let maybe_send_traces ~force (self : state) =
    send_batch_ self ~force self.traces ~mk_to_send:(fun spans ->
        let traces =
          Opentelemetry_client.Util_resources.make_resource_spans spans
        in
        To_send.Send_trace [ traces ])

  let[@inline] push_to_batch b e =
    if e <> [] then (
      match Batch.push b e with
      | `Ok -> ()
      | `Dropped -> Atomic.incr n_dropped
    )

  let create ~stop ~config () : #t =
    let open Opentelemetry_util in
    let st = create_state ~stop ~config () in
    let ticker = Cb_set.create () in
    object (self : #t)
      method send_trace spans =
        push_to_batch st.traces spans;
        maybe_send_traces st ~force:false

      method send_metrics m =
        push_to_batch st.metrics m;
        maybe_send_metrics st ~force:false

      method send_logs m =
        push_to_batch st.logs m;
        maybe_send_logs st ~force:false

      method add_on_tick_callback cb = Cb_set.register ticker cb

      method tick () = Cb_set.trigger ticker

      method cleanup ~on_done () : unit =
        if not (Atomic.exchange st.cleaned true) then (
          (* flush all signals *)
          maybe_send_logs ~force:true st;
          maybe_send_metrics ~force:true st;
          maybe_send_traces ~force:true st;

          (* close send queues, then wait for all threads *)
          Sync_queue.close st.send_q;
          Array.iter Thread.join st.send_threads
        );
        on_done ()

      method shutdown ~on_done () =
        Atomic.set st.stop true;
        self#cleanup ~on_done ()
    end

  let shutdown (self : #t) ~on_done : unit = self#shutdown ~on_done ()
end

let create_exporter ?(stop = Atomic.make false)
    ?(config : Config.t = Config.make ()) () : #OTEL.Exporter.t =
  let backend = Exporter_impl.create ~stop ~config () in
  (backend :> OTEL.Exporter.t)

(** thread that calls [tick()] regularly, to help enforce timeouts *)
let setup_ticker_thread ~stop ~sleep_ms (exp : #OTEL.Exporter.t) () =
  let sleep_s = float sleep_ms /. 1000. in
  let tick_loop () =
    try
      while not @@ Atomic.get stop do
        Thread.delay sleep_s;
        exp#tick ()
      done
    with
    | Sync_queue.Closed -> ()
    | exn ->
      (* print and ignore *)
      Printf.eprintf "otel-ocurl: ticker thread: uncaught exn:\n%s\n%!"
        (Printexc.to_string exn)
  in
  start_bg_thread tick_loop

let setup_ ?(stop = Atomic.make false) ?(config : Config.t = Config.make ()) ()
    : unit =
  let exporter = Exporter_impl.create ~stop ~config () in
  OTEL.Exporter.Main_exporter.set exporter;

  Self_trace.set_enabled config.common.self_trace;

  if config.ticker_thread then (
    (* at most a minute *)
    let sleep_ms = min 60_000 (max 2 config.ticker_interval_ms) in
    ignore (setup_ticker_thread ~stop ~sleep_ms exporter () : Thread.t)
  )

let remove_backend () : unit =
  (* we don't need the callback, this runs in the same thread *)
  OTEL.Exporter.Main_exporter.remove () ~on_done:ignore

let setup ?stop ?config ?(enable = true) () =
  if enable then setup_ ?stop ?config ()

let with_setup ?stop ?config ?(enable = true) () f =
  if enable then (
    setup_ ?stop ?config ();
    Fun.protect ~finally:remove_backend f
  ) else
    f ()

let[@inline] n_bytes_sent () = Atomic.get Exporter_impl.n_bytes_sent
