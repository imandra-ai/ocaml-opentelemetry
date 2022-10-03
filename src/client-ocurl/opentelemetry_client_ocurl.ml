(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
open Opentelemetry
include Common_

let needs_gc_metrics = Atomic.make false

let gc_metrics = AList.make ()
(* side channel for GC, appended to {!E_metrics}'s data *)

(* capture current GC metrics if {!needs_gc_metrics} is true,
   and push them into {!gc_metrics} for later
   collection *)
let sample_gc_metrics_if_needed () =
  if Atomic.compare_and_set needs_gc_metrics true false then (
    let l =
      OT.Metrics.make_resource_metrics
        ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
      @@ Opentelemetry.GC_metrics.get_metrics ()
    in
    AList.add gc_metrics l
  )

module Config = Config

let _init_curl =
  lazy
    (Curl.global_init Curl.CURLINIT_GLOBALALL;
     at_exit Curl.global_cleanup)

type error =
  [ `Status of int * Opentelemetry.Proto.Status.status
  | `Failure of string
  | `Sysbreak
  ]

let n_errors = Atomic.make 0

let n_dropped = Atomic.make 0

let report_err_ = function
  | `Sysbreak -> Printf.eprintf "opentelemetry: ctrl-c captured, stopping\n%!"
  | `Failure msg ->
    Format.eprintf "@[<2>opentelemetry: export failed: %s@]@." msg
  | `Status (code, { Opentelemetry.Proto.Status.code = scode; message; details })
    ->
    let pp_details out l =
      List.iter
        (fun s -> Format.fprintf out "%S;@ " (Bytes.unsafe_to_string s))
        l
    in
    Format.eprintf
      "@[<2>opentelemetry: export failed with@ http code=%d@ status \
       {@[code=%ld;@ message=%S;@ details=[@[%a@]]@]}@]@."
      code scode
      (Bytes.unsafe_to_string message)
      pp_details details

module Httpc : sig
  type t

  val create : unit -> t

  val send :
    t ->
    path:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result

  val cleanup : t -> unit
end = struct
  open Opentelemetry.Proto

  let () = Lazy.force _init_curl

  (* TODO: use Curl.Multi, etc. instead? *)
  type t = {
    buf_res: Buffer.t;
    curl: Curl.t;
  }

  let create () : t = { buf_res = Buffer.create 256; curl = Curl.init () }

  let cleanup self = Curl.cleanup self.curl

  (* send the content to the remote endpoint/path *)
  let send (self : t) ~path ~decode (bod : string) : ('a, error) result =
    let { curl; buf_res } = self in
    Curl.reset curl;
    if !debug_ then Curl.set_verbose curl true;
    let full_url = !url ^ path in
    Curl.set_url curl full_url;
    Curl.set_httppost curl [];
    let to_http_header (k, v) = Printf.sprintf "%s: %s" k v in
    let http_headers = List.map to_http_header !headers in
    Curl.set_httpheader curl
      ("Content-Type: application/x-protobuf" :: http_headers);
    (* write body *)
    Curl.set_post curl true;
    Curl.set_postfieldsize curl (String.length bod);
    Curl.set_readfunction curl
      (let i = ref 0 in
       fun n ->
         if !debug_ then Printf.eprintf "curl asks for %d bytes\n%!" n;
         let len = min n (String.length bod - !i) in
         let s = String.sub bod !i len in
         if !debug_ then Printf.eprintf "gave curl %d bytes\n%!" len;
         i := !i + len;
         s);
    (* read result's body *)
    Buffer.clear buf_res;
    Curl.set_writefunction curl (fun s ->
        Buffer.add_string buf_res s;
        String.length s);
    try
      match Curl.perform curl with
      | () ->
        let code = Curl.get_responsecode curl in
        if !debug_ then
          Printf.eprintf "result body: %S\n%!" (Buffer.contents buf_res);
        if code >= 200 && code < 300 then (
          match decode with
          | `Ret x -> Ok x
          | `Dec f ->
            let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
            (try Ok (f dec)
             with e ->
               let bt = Printexc.get_backtrace () in
               Error
                 (`Failure
                   (spf "decoding failed with:\n%s\n%s" (Printexc.to_string e)
                      bt)))
        ) else (
          let str = Buffer.contents buf_res in
          let dec = Pbrt.Decoder.of_string str in

          try
            let status = Status.decode_status dec in
            Error (`Status (code, status))
          with e ->
            let bt = Printexc.get_backtrace () in
            Error
              (`Failure
                (spf
                   "httpc: decoding of status (url=%S, code=%d) failed with:\n\
                    %s\n\
                    status: %S\n\
                    %s"
                   full_url code (Printexc.to_string e) str bt))
        )
      | exception Sys.Break -> Error `Sysbreak
      | exception Curl.CurlException (_, code, msg) ->
        let status =
          Status.default_status ~code:(Int32.of_int code)
            ~message:(Bytes.unsafe_of_string msg)
            ()
        in
        Error (`Status (code, status))
    with
    | Sys.Break -> Error `Sysbreak
    | e ->
      let bt = Printexc.get_backtrace () in
      Error
        (`Failure
          (spf "httpc: post on url=%S failed with:\n%s\n%s" full_url
             (Printexc.to_string e) bt))
end

(** Batch of resources to be pushed later.

    This type is thread-safe. *)
module Batch : sig
  type 'a t

  val push : 'a t -> 'a -> bool
  (** [push batch x] pushes [x] into the batch, and heuristically
      returns [true] if the batch is ready to be emitted (to know if we should
      wake up the sending thread, if any) *)

  val push' : 'a t -> 'a -> unit

  val is_ready : now:Mtime.t -> _ t -> bool
  (** is the batch ready to be sent? This is heuristic. *)

  val pop_if_ready : ?force:bool -> now:Mtime.t -> 'a t -> 'a list option
  (** Is the batch ready to be emitted? If batching is disabled,
      this is true as soon as {!is_empty} is false. If a timeout is provided
      for this batch, then it will be ready if an element has been in it
      for at least the timeout.
      @param now passed to implement timeout *)

  val make : ?batch:int -> ?timeout:Mtime.span -> unit -> 'a t
  (** Create a new batch *)
end = struct
  type 'a t = {
    lock: Mutex.t;
    mutable size: int;
    mutable q: 'a list;
    batch: int option;
    high_watermark: int;
    timeout: Mtime.span option;
    mutable start: Mtime.t;
  }

  let make ?batch ?timeout () : _ t =
    Option.iter (fun b -> assert (b > 0)) batch;
    let high_watermark = Option.fold ~none:100 ~some:(fun x -> x * 10) batch in
    {
      lock = Mutex.create ();
      size = 0;
      start = Mtime_clock.now ();
      q = [];
      batch;
      timeout;
      high_watermark;
    }

  let timeout_expired_ ~now self : bool =
    match self.timeout with
    | Some t ->
      let elapsed = Mtime.span now self.start in
      Mtime.Span.compare elapsed t >= 0
    | None -> false

  let is_full_ self : bool =
    match self.batch with
    | None -> self.size > 0
    | Some b -> self.size >= b

  let is_ready ~now self : bool =
    let@ () = with_mutex_ self.lock in
    is_full_ self || timeout_expired_ ~now self

  let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
    let@ () = with_mutex_ self.lock in
    if self.size > 0 && (force || is_full_ self || timeout_expired_ ~now self)
    then (
      let l = self.q in
      self.q <- [];
      self.size <- 0;
      assert (l <> []);
      Some l
    ) else
      None

  let push (self : _ t) x : bool =
    let@ () = with_mutex_ self.lock in
    if self.size >= self.high_watermark then (
      (* drop this to prevent queue from growing too fast *)
      Atomic.incr n_dropped;
      true
    ) else (
      if self.size = 0 && Option.is_some self.timeout then
        (* current batch starts now *)
        self.start <- Mtime_clock.now ();

      (* add to queue *)
      self.size <- 1 + self.size;
      self.q <- x :: self.q;
      let ready = is_full_ self in
      ready
    )

  let push' self x = ignore (push self x : bool)
end

(** An emitter. This is used by {!Backend} below to forward traces/metrics/…
    from the program to whatever collector client we have. *)
module type EMITTER = sig
  open Opentelemetry.Proto

  val push_trace : Trace.resource_spans list -> unit

  val push_metrics : Metrics.resource_metrics list -> unit

  val push_logs : Logs.resource_logs list -> unit

  val set_on_tick_callbacks : (unit -> unit) list ref -> unit

  val tick : unit -> unit

  val cleanup : unit -> unit
end

(* start a thread in the background, running [f()] *)
let start_bg_thread (f : unit -> unit) : unit =
  let run () =
    (* block some signals: USR1 USR2 TERM PIPE ALARM STOP, see [$ kill -L] *)
    ignore (Thread.sigmask Unix.SIG_BLOCK [ 10; 12; 13; 14; 15; 19 ] : _ list);
    f ()
  in
  ignore (Thread.create run () : Thread.t)

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~stop ~(config : Config.t) () : (module EMITTER) =
  let open Proto in
  (* local helpers *)
  let open struct
    let timeout =
      if config.batch_timeout_ms > 0 then
        Some Mtime.Span.(config.batch_timeout_ms * ms)
      else
        None

    let batch_traces : Trace.resource_spans list Batch.t =
      Batch.make ?batch:config.batch_traces ?timeout ()

    let batch_metrics : Metrics.resource_metrics list Batch.t =
      Batch.make ?batch:config.batch_metrics ?timeout ()

    let batch_logs : Logs.resource_logs list Batch.t =
      Batch.make ?batch:config.batch_logs ?timeout ()

    let on_tick_cbs_ = Atomic.make (ref [])

    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let send_http_ (httpc : Httpc.t) encoder ~path ~encode x : unit =
      Pbrt.Encoder.reset encoder;
      encode x encoder;
      let data = Pbrt.Encoder.to_string encoder in
      match Httpc.send httpc ~path ~decode:(`Ret ()) data with
      | Ok () -> ()
      | Error `Sysbreak ->
        Printf.eprintf "ctrl-c captured, stopping\n%!";
        Atomic.set stop true
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err;
        (* avoid crazy error loop *)
        Thread.delay 3.

    let send_metrics_http curl encoder (l : Metrics.resource_metrics list list)
        =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Metrics_service.default_export_metrics_service_request
          ~resource_metrics:l ()
      in
      send_http_ curl encoder ~path:"/v1/metrics"
        ~encode:Metrics_service.encode_export_metrics_service_request x

    let send_traces_http curl encoder (l : Trace.resource_spans list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Trace_service.default_export_trace_service_request ~resource_spans:l ()
      in
      send_http_ curl encoder ~path:"/v1/traces"
        ~encode:Trace_service.encode_export_trace_service_request x

    let send_logs_http curl encoder (l : Logs.resource_logs list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Logs_service.default_export_logs_service_request ~resource_logs:l ()
      in
      send_http_ curl encoder ~path:"/v1/logs"
        ~encode:Logs_service.encode_export_logs_service_request x

    (* emit metrics, if the batch is full or timeout lapsed *)
    let emit_metrics_maybe ~now ?force httpc encoder : bool =
      match Batch.pop_if_ready ?force ~now batch_metrics with
      | None -> false
      | Some l ->
        let batch = AList.pop_all gc_metrics :: l in
        send_metrics_http httpc encoder batch;
        true

    let emit_traces_maybe ~now ?force httpc encoder : bool =
      match Batch.pop_if_ready ?force ~now batch_traces with
      | None -> false
      | Some l ->
        send_traces_http httpc encoder l;
        true

    let emit_logs_maybe ~now ?force httpc encoder : bool =
      match Batch.pop_if_ready ?force ~now batch_logs with
      | None -> false
      | Some l ->
        send_logs_http httpc encoder l;
        true

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf
          "opentelemetry-curl: uncaught exception in %s: %s\n%s\n%!" where
          (Printexc.to_string e) bt

    let emit_all_force (httpc : Httpc.t) encoder =
      let now = Mtime_clock.now () in
      ignore (emit_traces_maybe ~now ~force:true httpc encoder : bool);
      ignore (emit_logs_maybe ~now ~force:true httpc encoder : bool);
      ignore (emit_metrics_maybe ~now ~force:true httpc encoder : bool)

    let tick_common_ () =
      if !debug_ then Printf.eprintf "tick (from %d)\n%!" (tid ());
      sample_gc_metrics_if_needed ();
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        !(Atomic.get on_tick_cbs_);
      ()

    let setup_ticker_thread ~tick ~finally () =
      (* thread that calls [tick()] regularly, to help enforce timeouts *)
      let tick_thread () =
        let@ () =
          Fun.protect ~finally:(fun () ->
              Atomic.set stop true;
              finally ())
        in
        while not @@ Atomic.get stop do
          Thread.delay 0.5;
          tick ()
        done
      in
      start_bg_thread tick_thread
  end in
  (* setup a global lock *)
  (let global_lock_ = Mutex.create () in
   Lock.set_mutex
     ~lock:(fun () -> Mutex.lock global_lock_)
     ~unlock:(fun () -> Mutex.unlock global_lock_));

  if config.bg_threads > 0 then (
    (* lock+condition used for background threads to wait, and be woken up
       when a batch is ready *)
    let m = Mutex.create () in
    let cond = Condition.create () in

    (* loop for the thread that processes events and sends them to collector *)
    let bg_thread () =
      let httpc = Httpc.create () in
      let encoder = Pbrt.Encoder.create () in
      while not @@ Atomic.get stop do
        let@ () = guard_exn_ (spf "bg thread[%d] (main loop)" @@ tid ()) in

        let now = Mtime_clock.now () in
        let do_metrics = emit_metrics_maybe ~now httpc encoder in
        let do_traces = emit_traces_maybe ~now httpc encoder in
        let do_logs = emit_logs_maybe ~now httpc encoder in
        if (not do_metrics) && (not do_traces) && not do_logs then (
          let@ () = guard_exn_ (spf "bg thread[%d] (waiting)" @@ tid ()) in
          (* wait for something to happen *)
          Mutex.lock m;
          Condition.wait cond m;
          Mutex.unlock m
        )
      done;
      (* flush remaining events once we exit *)
      let@ () = guard_exn_ "bg thread (cleanup)" in
      emit_all_force httpc encoder;
      Httpc.cleanup httpc
    in

    for _i = 1 to config.bg_threads do
      start_bg_thread bg_thread
    done;

    (* if the bg thread waits, this will wake it up so it can send batches *)
    let wakeup ~all () =
      with_mutex_ m (fun () ->
          if all then
            Condition.broadcast cond
          else
            Condition.signal cond);
      Thread.yield ()
    in

    let tick () =
      tick_common_ ();

      let now = Mtime_clock.now () in
      if Atomic.get stop then
        wakeup ~all:true ()
      else if
        Batch.is_ready ~now batch_metrics
        || Batch.is_ready ~now batch_traces
        || Batch.is_ready ~now batch_logs
      then
        wakeup ~all:false ()
    in

    if config.ticker_thread then
      setup_ticker_thread ~tick ~finally:(fun () -> wakeup ~all:true ()) ();

    let module M = struct
      let push_trace e = if Batch.push batch_traces e then wakeup ~all:false ()

      let push_metrics e =
        if Batch.push batch_metrics e then wakeup ~all:false ()

      let push_logs e = if Batch.push batch_logs e then wakeup ~all:false ()

      let set_on_tick_callbacks = set_on_tick_callbacks

      let tick = tick

      let cleanup () =
        Atomic.set stop true;
        if !debug_ then Printf.eprintf "opentelemetry: exiting…\n%!";
        wakeup ~all:true ()
    end in
    (module M)
  ) else (
    let httpc = Httpc.create () in
    let encoder = Pbrt.Encoder.create () in

    let module M = struct
      (* we make sure that this is thread-safe, even though we don't have a
         background thread. There can still be a ticker thread, and there
         can also be several user threads that produce spans and call
         the emit functions. *)

      let push_trace e =
        let@ () = guard_exn_ "push trace" in
        Batch.push' batch_traces e;
        let now = Mtime_clock.now () in
        let@ () = Lock.with_lock in
        ignore (emit_traces_maybe ~now httpc encoder : bool)

      let push_metrics e =
        let@ () = guard_exn_ "push metrics" in
        sample_gc_metrics_if_needed ();
        Batch.push' batch_metrics e;
        let now = Mtime_clock.now () in
        let@ () = Lock.with_lock in
        ignore (emit_metrics_maybe ~now httpc encoder : bool)

      let push_logs e =
        let@ () = guard_exn_ "push logs" in
        Batch.push' batch_logs e;
        let now = Mtime_clock.now () in
        let@ () = Lock.with_lock in
        ignore (emit_logs_maybe ~now httpc encoder : bool)

      let set_on_tick_callbacks = set_on_tick_callbacks

      let tick () =
        tick_common_ ();
        sample_gc_metrics_if_needed ();
        let@ () = Lock.with_lock in
        let now = Mtime_clock.now () in
        ignore (emit_traces_maybe ~now httpc encoder : bool);
        ignore (emit_metrics_maybe ~now httpc encoder : bool);
        ignore (emit_logs_maybe ~now httpc encoder : bool);
        ()

      (* make sure we have a ticker thread, if required  *)
      let () =
        if config.ticker_thread then
          setup_ticker_thread ~tick ~finally:ignore ()

      let cleanup () =
        if !debug_ then Printf.eprintf "opentelemetry: exiting…\n%!";
        emit_all_force httpc encoder;
        Httpc.cleanup httpc
    end in
    (module M)
  )

module Backend (Arg : sig
  val stop : bool Atomic.t

  val config : Config.t
end)
() : Opentelemetry.Collector.BACKEND = struct
  include (val mk_emitter ~stop:Arg.stop ~config:Arg.config ())

  open Opentelemetry.Proto
  open Opentelemetry.Collector

  let send_trace : Trace.resource_spans list sender =
    {
      send =
        (fun l ~ret ->
          (if !debug_ then
            let@ () = Lock.with_lock in
            Format.eprintf "send spans %a@."
              (Format.pp_print_list Trace.pp_resource_spans)
              l);
          push_trace l;
          ret ());
    }

  let last_sent_metrics = Atomic.make (Mtime_clock.now ())

  let timeout_sent_metrics = Mtime.Span.(5 * s)
  (* send metrics from time to time *)

  let signal_emit_gc_metrics () =
    if !debug_ then
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
          (if !debug_ then
            let@ () = Lock.with_lock in
            Format.eprintf "send metrics %a@."
              (Format.pp_print_list Metrics.pp_resource_metrics)
              m);

          let m = List.rev_append (additional_metrics ()) m in
          push_metrics m;
          ret ());
    }

  let send_logs : Logs.resource_logs list sender =
    {
      send =
        (fun m ~ret ->
          (if !debug_ then
            let@ () = Lock.with_lock in
            Format.eprintf "send logs %a@."
              (Format.pp_print_list Logs.pp_resource_logs)
              m);

          push_logs m;
          ret ());
    }
end

let setup_ ?(stop = Atomic.make false) ~(config : Config.t) () =
  debug_ := config.debug;
  let module B =
    Backend
      (struct
        let stop = stop

        let config = config
      end)
      ()
  in
  Opentelemetry.Collector.set_backend (module B);
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
