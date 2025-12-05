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

let n_bytes_sent : int Atomic.t = Atomic.make 0

module Consumer_impl = struct
  type state = {
    bq: Any_resource.t Bounded_queue.t;  (** Queue of incoming workload *)
    stop: bool Atomic.t;
    config: Config.t;
    mutable send_threads: Thread.t array;
        (** Threads that send data via http *)
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    mcond: Util_thread.MCond.t;  (** how to wait for the queue *)
  }

  let shutdown self : unit =
    Atomic.set self.stop true;
    (* wakeup sleepers *)
    Util_thread.MCond.signal self.mcond

  let send_http_ (self : state) (client : Curl.t) ~url (data : string) : unit =
    let@ _sc =
      Self_trace.with_ ~kind:Span_kind_producer "otel-ocurl.send-http"
    in

    if Config.Env.get_debug () then
      Printf.eprintf "opentelemetry: send http POST to %s (%dB)\n%!" url
        (String.length data);
    let headers =
      ("Content-Type", "application/x-protobuf") :: self.config.common.headers
    in
    match
      let@ _sc =
        Self_trace.with_ ~kind:Span_kind_internal "curl.post"
          ~attrs:[ "size", `Int (String.length data); "url", `String url ]
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
        let err = Export_error.decode_invalid_http_response ~url ~code body in
        Export_error.report_err err;
        ()
      )
    | exception Sys.Break ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      shutdown self
    | Error (code, msg) ->
      (* TODO: log error _via_ otel? *)
      Atomic.incr n_errors;

      Printf.eprintf
        "opentelemetry: export failed:\n  %s\n  curl code: %s\n  url: %s\n%!"
        msg (Curl.strerror code) url;

      (* avoid crazy error loop *)
      Thread.delay 3.

  (** The main loop of a thread that, reads from [bq] to get the next message to
      send via http *)
  let bg_thread_loop (self : state) : unit =
    Ezcurl.with_client ?set_opts:None @@ fun client ->
    (* we need exactly one encoder per thread *)
    let encoder = Pbrt.Encoder.create ~size:2048 () in

    let send ~name ~url ~conv (signals : _ list) : unit =
      let@ _sp =
        Self_trace.with_ ~kind:Span_kind_producer name
          ~attrs:[ "n", `Int (List.length signals) ]
      in

      let msg : string = conv ?encoder:(Some encoder) signals in
      Pbrt.Encoder.reset encoder;

      ignore (Atomic.fetch_and_add n_bytes_sent (String.length msg) : int);
      send_http_ self client msg ~url;
      ()
    in
    while not (Atomic.get self.stop) do
      match Bounded_queue.try_pop self.bq with
      | `Closed -> shutdown self
      | `Empty -> Util_thread.MCond.wait self.mcond
      | `Item (Any_resource.R_spans tr) ->
        send ~name:"send-traces" ~conv:Signal.Encode.traces
          ~url:self.config.common.url_traces tr
      | `Item (Any_resource.R_metrics ms) ->
        send ~name:"send-metrics" ~conv:Signal.Encode.metrics
          ~url:self.config.common.url_metrics ms
      | `Item (Any_resource.R_logs logs) ->
        send ~name:"send-logs" ~conv:Signal.Encode.logs
          ~url:self.config.common.url_logs logs
    done

  let to_consumer (self : state) : _ Consumer.t =
    let active () = not (Atomic.get self.stop) in
    let tick () =
      (* make sure to poll from time to time *)
      Util_thread.MCond.signal self.mcond
    in
    let shutdown ~on_done =
      shutdown self;
      on_done ()
    in
    { tick; active; shutdown }

  let create_state ~stop ~(config : Config.t) ~q () : state =
    let n_send_threads = min 100 @@ max 2 config.bg_threads in

    let self =
      {
        stop;
        config;
        send_threads = [||];
        bq = q;
        cleaned = Atomic.make false;
        mcond = Util_thread.MCond.create ();
      }
    in

    Util_thread.MCond.wakeup_from_bq self.mcond q;

    self.send_threads <-
      Array.init n_send_threads (fun _i ->
          Util_thread.start_bg_thread (fun () -> bg_thread_loop self));

    self

  let create ~stop ~config () : Consumer.any_resource_builder =
    {
      start_consuming =
        (fun q ->
          let st = create_state ~stop ~config ~q () in
          to_consumer st);
    }
end

let consumer ?(stop = Atomic.make false) ?(config = Config.make ()) () :
    Opentelemetry_client.Consumer.any_resource_builder =
  Consumer_impl.create ~stop ~config ()

let create_exporter ?stop ?(config = Config.make ()) () : OTEL.Exporter.t =
  let consumer = consumer ?stop ~config () in
  let bq =
    Bounded_queue_sync.create
      ~high_watermark:Bounded_queue.Defaults.high_watermark ()
  in

  Exporter_queued.create ~q:bq ~consumer ()
  |> Exporter_add_batching.add_batching ~config:config.common

let create_backend = create_exporter

let setup_ ?(stop = Atomic.make false) ?(config : Config.t = Config.make ()) ()
    : unit =
  let exporter = create_exporter ~stop ~config () in
  OTEL.Main_exporter.set exporter;

  Self_trace.set_enabled config.common.self_trace;

  if config.ticker_thread then (
    (* at most a minute *)
    let sleep_ms = min 60_000 (max 2 config.ticker_interval_ms) in
    ignore
      (Util_thread.setup_ticker_thread ~stop ~sleep_ms exporter () : Thread.t)
  )

let remove_backend () : unit =
  (* we don't need the callback, this runs in the same thread *)
  OTEL.Main_exporter.remove () ~on_done:ignore

let setup ?stop ?config ?(enable = true) () =
  if enable then setup_ ?stop ?config ()

let with_setup ?stop ?config ?(enable = true) () f =
  if enable then (
    setup_ ?stop ?config ();
    Fun.protect ~finally:remove_backend f
  ) else
    f ()

let[@inline] n_bytes_sent () = Atomic.get n_bytes_sent
