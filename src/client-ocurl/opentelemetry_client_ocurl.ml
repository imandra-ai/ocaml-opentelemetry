(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
module Config = Config
open Opentelemetry
include Common_

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

(** side channel for GC, appended to metrics batch data *)
let gc_metrics = AList.make ()

(** capture current GC metrics if {!needs_gc_metrics} is true
    or it has been a long time since the last GC metrics collection,
    and push them into {!gc_metrics} for later collection *)
let sample_gc_metrics_if_needed () =
  let now = Mtime_clock.now () in
  let alarm = Atomic.exchange needs_gc_metrics false in
  let timeout () =
    let elapsed = Mtime.span now (Atomic.get last_gc_metrics) in
    Mtime.Span.compare elapsed timeout_gc_metrics > 0
  in
  if alarm || timeout () then (
    Atomic.set last_gc_metrics now;
    let l =
      OT.Metrics.make_resource_metrics
        ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
      @@ Opentelemetry.GC_metrics.get_metrics ()
    in
    AList.add gc_metrics l
  )

let n_errors = Atomic.make 0

let n_dropped = Atomic.make 0

(** Something sent to the collector *)
module Event = struct
  open Opentelemetry.Proto

  type t =
    | E_metric of Metrics.resource_metrics list
    | E_trace of Trace.resource_spans list
    | E_logs of Logs.resource_logs list
    | E_tick
    | E_flush_all  (** Flush all batches *)
end

(** Something to be sent via HTTP *)
module To_send = struct
  open Opentelemetry.Proto

  type t =
    | Send_metric of Metrics.resource_metrics list list
    | Send_trace of Trace.resource_spans list list
    | Send_logs of Logs.resource_logs list list
end

(** start a thread in the background, running [f()] *)
let start_bg_thread (f : unit -> unit) : Thread.t =
  let run () =
    (* block some signals: USR1 USR2 TERM PIPE ALARM STOP, see [$ kill -L] *)
    ignore (Thread.sigmask Unix.SIG_BLOCK [ 10; 12; 13; 14; 15; 19 ] : _ list);
    f ()
  in
  Thread.create run ()

let str_to_hex (s : string) : string =
  let i_to_hex (i : int) =
    if i < 10 then
      Char.chr (i + Char.code '0')
    else
      Char.chr (i - 10 + Char.code 'a')
  in

  let res = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    let n = Char.code (String.get s i) in
    Bytes.set res (2 * i) (i_to_hex ((n land 0xf0) lsr 4));
    Bytes.set res ((2 * i) + 1) (i_to_hex (n land 0x0f))
  done;
  Bytes.unsafe_to_string res

module Backend_impl : sig
  type t

  val create : stop:bool Atomic.t -> config:Config.t -> unit -> t

  val send_event : t -> Event.t -> unit

  val shutdown : t -> unit
end = struct
  open Opentelemetry.Proto

  type t = {
    stop: bool Atomic.t;
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    config: Config.t;
    q: Event.t B_queue.t;  (** Queue to receive data from the user's code *)
    mutable main_th: Thread.t option;  (** Thread that listens on [q] *)
    send_q: To_send.t B_queue.t;  (** Queue for the send worker threads *)
    mutable send_threads: Thread.t array;  (** Threads that send data via http *)
  }

  let send_http_ ~stop ~config (client : Curl.t) encoder ~path ~encode x : unit
      =
    Pbrt.Encoder.reset encoder;
    encode x encoder;
    let data = Pbrt.Encoder.to_string encoder in
    let url = config.Config.url ^ path in
    if !debug_ || config.debug then
      Printf.eprintf "opentelemetry: send http POST to %s (%dB)\n%!" url
        (String.length data);
    let headers =
      ("Content-Type", "application/x-protobuf") :: config.headers
    in
    match
      Ezcurl.post ~headers ~client ~params:[] ~url ~content:(`String data) ()
    with
    | Ok { code; _ } when code >= 200 && code < 300 -> ()
    | Ok { code; body; headers = _; info = _ } ->
      Atomic.incr n_errors;
      if !debug_ || config.debug then (
        let dec = Pbrt.Decoder.of_string body in
        let body =
          try
            let status = Status.decode_status dec in
            Format.asprintf "%a" Status.pp_status status
          with _ ->
            spf "(could not decode status)\nraw bytes: %s" (str_to_hex body)
        in
        Printf.eprintf "error while sending:\n  code=%d\n  %s\n%!" code body
      );
      ()
    | exception Sys.Break ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      Atomic.set stop true
    | Error (code, msg) ->
      (* TODO: log error _via_ otel? *)
      Atomic.incr n_errors;

      Printf.eprintf "opentelemetry: export failed:\n  %s\n  curl code: %s\n%!"
        msg (Curl.strerror code);

      (* avoid crazy error loop *)
      Thread.delay 3.

  let send_logs_http ~stop ~config (client : Curl.t) encoder
      (l : Logs.resource_logs list list) : unit =
    let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    let x =
      Logs_service.default_export_logs_service_request ~resource_logs:l ()
    in
    send_http_ ~stop ~config client encoder ~path:"/v1/logs"
      ~encode:Logs_service.encode_export_logs_service_request x

  let send_metrics_http ~stop ~config curl encoder
      (l : Metrics.resource_metrics list list) : unit =
    let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    let x =
      Metrics_service.default_export_metrics_service_request ~resource_metrics:l
        ()
    in
    send_http_ ~stop ~config curl encoder ~path:"/v1/metrics"
      ~encode:Metrics_service.encode_export_metrics_service_request x

  let send_traces_http ~stop ~config curl encoder
      (l : Trace.resource_spans list list) : unit =
    let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    let x =
      Trace_service.default_export_trace_service_request ~resource_spans:l ()
    in
    send_http_ ~stop ~config curl encoder ~path:"/v1/traces"
      ~encode:Trace_service.encode_export_trace_service_request x

  let[@inline] send_event (self : t) ev : unit = B_queue.push self.q ev

  (** Thread that, in a loop, reads from [q] to get the
      next message to send via http *)
  let bg_thread_loop (self : t) : unit =
    Ezcurl.with_client ?set_opts:None @@ fun client ->
    let stop = self.stop in
    let config = self.config in
    let encoder = Pbrt.Encoder.create () in
    try
      while not (Atomic.get stop) do
        let msg = B_queue.pop self.send_q in
        match msg with
        | To_send.Send_trace tr ->
          send_traces_http ~stop ~config client encoder tr
        | To_send.Send_metric ms ->
          send_metrics_http ~stop ~config client encoder ms
        | To_send.Send_logs logs ->
          send_logs_http ~stop ~config client encoder logs
      done
    with B_queue.Closed -> ()

  type batches = {
    traces: Proto.Trace.resource_spans Batch.t;
    logs: Proto.Logs.resource_logs Batch.t;
    metrics: Proto.Metrics.resource_metrics Batch.t;
  }

  let batch_max_size_ = 200

  let should_send_batch_ ~config ~now (b : _ Batch.t) : bool =
    Batch.len b > 0
    && (Batch.len b >= batch_max_size_
       ||
       let timeout = Mtime.Span.(config.Config.batch_timeout_ms * ms) in
       let elapsed = Mtime.span now (Batch.time_started b) in
       Mtime.Span.compare elapsed timeout >= 0)

  let main_thread_loop (self : t) : unit =
    let local_q = Queue.create () in
    let config = self.config in

    (* keep track of batches *)
    let batches =
      {
        traces = Batch.create ();
        logs = Batch.create ();
        metrics = Batch.create ();
      }
    in

    let send_metrics () =
      B_queue.push self.send_q
        (To_send.Send_metric (Batch.pop_all batches.metrics))
    in

    let send_logs () =
      B_queue.push self.send_q (To_send.Send_logs (Batch.pop_all batches.logs))
    in

    let send_traces () =
      B_queue.push self.send_q
        (To_send.Send_trace (Batch.pop_all batches.traces))
    in

    try
      while not (Atomic.get self.stop) do
        (* read multiple events at once *)
        B_queue.pop_all self.q local_q;

        let now = Mtime_clock.now () in

        (* how to process a single event *)
        let process_ev (ev : Event.t) : unit =
          match ev with
          | Event.E_metric m ->
            Batch.push batches.metrics m;
            if should_send_batch_ ~config ~now batches.metrics then
              send_metrics ()
          | Event.E_trace tr ->
            Batch.push batches.traces tr;
            if should_send_batch_ ~config ~now batches.traces then
              send_traces ()
          | Event.E_logs logs ->
            Batch.push batches.logs logs;
            if should_send_batch_ ~config ~now batches.logs then send_logs ()
          | Event.E_tick ->
            (* check for batches whose timeout expired *)
            if should_send_batch_ ~config ~now batches.metrics then
              send_metrics ();
            if should_send_batch_ ~config ~now batches.logs then send_logs ();
            if should_send_batch_ ~config ~now batches.traces then
              send_traces ()
          | Event.E_flush_all ->
            if Batch.len batches.metrics > 0 then send_metrics ();
            if Batch.len batches.logs > 0 then send_logs ();
            if Batch.len batches.traces > 0 then send_traces ()
        in

        while not (Queue.is_empty local_q) do
          let ev = Queue.pop local_q in
          process_ev ev
        done
      done
    with B_queue.Closed -> ()

  let create ~stop ~config () : t =
    let n_send_threads = max 2 config.Config.bg_threads in
    let self =
      {
        stop;
        config;
        q = B_queue.create ();
        send_threads = [||];
        send_q = B_queue.create ();
        cleaned = Atomic.make false;
        main_th = None;
      }
    in

    let main_th = start_bg_thread (fun () -> main_thread_loop self) in
    self.main_th <- Some main_th;

    self.send_threads <-
      Array.init n_send_threads (fun _i ->
          start_bg_thread (fun () -> bg_thread_loop self));

    self

  let shutdown self : unit =
    Atomic.set self.stop true;
    if not (Atomic.exchange self.cleaned true) then (
      (* empty batches *)
      send_event self Event.E_flush_all;
      (* close the incoming queue, wait for the thread to finish
         before we start cutting off the background threads, so that they
         have time to receive the final batches *)
      B_queue.close self.q;
      Option.iter Thread.join self.main_th;
      (* close send queues, then wait for all threads *)
      B_queue.close self.send_q;
      Array.iter Thread.join self.send_threads
    )
end

let mk_backend ~stop ~config () : (module Collector.BACKEND) =
  let module M = struct
    open Opentelemetry.Proto
    open Opentelemetry.Collector

    let backend = Backend_impl.create ~stop ~config ()

    let send_trace : Trace.resource_spans list sender =
      {
        send =
          (fun l ~ret ->
            Backend_impl.send_event backend (Event.E_trace l);
            ret ());
      }

    let last_sent_metrics = Atomic.make (Mtime_clock.now ())

    (* send metrics from time to time *)
    let timeout_sent_metrics = Mtime.Span.(5 * s)

    let signal_emit_gc_metrics () =
      if !debug_ || config.debug then
        Printf.eprintf "opentelemetry: emit GC metrics requested\n%!";
      Atomic.set needs_gc_metrics true

    let additional_metrics () : Metrics.resource_metrics list =
      (* add exporter metrics to the lot? *)
      let last_emit = Atomic.get last_sent_metrics in
      let now = Mtime_clock.now () in
      let add_own_metrics =
        let elapsed = Mtime.span last_emit now in
        Mtime.Span.compare elapsed timeout_sent_metrics > 0
      in

      (* there is a possible race condition here, as several threads might update
         metrics at the same time. But that's harmless. *)
      if add_own_metrics then (
        Atomic.set last_sent_metrics now;
        let open OT.Metrics in
        [
          make_resource_metrics
            [
              sum ~name:"otel.export.dropped" ~is_monotonic:true
                [
                  int
                    ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                    ~now:(Mtime.to_uint64_ns now) (Atomic.get n_dropped);
                ];
              sum ~name:"otel.export.errors" ~is_monotonic:true
                [
                  int
                    ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                    ~now:(Mtime.to_uint64_ns now) (Atomic.get n_errors);
                ];
            ];
        ]
      ) else
        []

    let send_metrics : Metrics.resource_metrics list sender =
      {
        send =
          (fun m ~ret ->
            let m = List.rev_append (additional_metrics ()) m in
            Backend_impl.send_event backend (Event.E_metric m);
            ret ());
      }

    let send_logs : Logs.resource_logs list sender =
      {
        send =
          (fun m ~ret ->
            Backend_impl.send_event backend (Event.E_logs m);
            ret ());
      }

    let on_tick_cbs_ = Atomic.make (ref [])

    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let tick () =
      sample_gc_metrics_if_needed ();
      Backend_impl.send_event backend Event.E_tick;
      let l = Atomic.get on_tick_cbs_ in
      List.iter (fun f -> f ()) !l

    let cleanup () = Backend_impl.shutdown backend
  end in
  (module M)

(** thread that calls [tick()] regularly, to help enforce timeouts *)
let setup_ticker_thread ~stop ~sleep_ms (module B : Collector.BACKEND) () =
  let sleep_s = float sleep_ms /. 1000. in
  let tick_loop () =
    while not @@ Atomic.get stop do
      Thread.delay sleep_s;
      B.tick ()
    done
  in
  start_bg_thread tick_loop

let setup_ ?(stop = Atomic.make false) ~(config : Config.t) () =
  let ((module B) as backend) = mk_backend ~stop ~config () in
  Opentelemetry.Collector.set_backend backend;

  if config.ticker_thread then (
    let sleep_ms = min 5_000 (max 2 config.batch_timeout_ms) in
    ignore (setup_ticker_thread ~stop ~sleep_ms backend () : Thread.t)
  );

  B.cleanup

let setup ?stop ?(config = Config.make ()) ?(enable = true) () =
  if enable then (
    let cleanup = setup_ ?stop ~config () in
    at_exit cleanup
  )

let with_setup ?stop ?(config = Config.make ()) ?(enable = true) () f =
  if enable then (
    let cleanup = setup_ ?stop ~config () in
    Fun.protect ~finally:cleanup f
  ) else
    f ()