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

(* capture current GC metrics and push them into {!gc_metrics} for later
   collection *)
let sample_gc_metrics () =
  Atomic.set needs_gc_metrics false;
  let l =
    OT.Metrics.make_resource_metrics
      ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
    @@ Opentelemetry.GC_metrics.get_metrics ()
  in
  AList.add gc_metrics l

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
    decode:(Pbrt.Decoder.t -> 'a) ->
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
    Curl.set_url curl (!url ^ path);
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
        let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
        if code >= 200 && code < 300 then (
          let res = decode dec in
          Ok res
        ) else (
          let status = Status.decode_status dec in
          Error (`Status (code, status))
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
    | e -> Error (`Failure (Printexc.to_string e))
end

module type BATCH = sig end

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
end = struct
  type 'a t = {
    lock: Mutex.t;
    mutable size: int;
    mutable q: 'a list;
    batch: int option;
    timeout: Mtime.span option;
    mutable start: Mtime.t;
  }

  let make ?batch ?timeout () : _ t =
    Option.iter (fun b -> assert (b > 0)) batch;
    {
      lock = Mutex.create ();
      size = 0;
      start = Mtime_clock.now ();
      q = [];
      batch;
      timeout;
    }

  let is_empty_ self = self.size = 0

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
    if
      (force && not (is_empty_ self))
      || is_full_ self || timeout_expired_ ~now self
    then (
      let l = self.q in
      self.q <- [];
      self.size <- 0;
      Some l
    ) else
      None

  let push (self : _ t) x : bool =
    let@ () = with_mutex_ self.lock in
    if self.size = 0 && Option.is_some self.timeout then
      self.start <- Mtime_clock.now ();
    self.size <- 1 + self.size;
    self.q <- x :: self.q;
    let ready = is_full_ self in
    ready

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

let l_is_empty = function
  | [] -> true
  | _ :: _ -> false

let batch_is_empty = List.for_all l_is_empty

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

    let on_tick_cbs_ = ref (ref [])

    let set_on_tick_callbacks = ( := ) on_tick_cbs_

    let send_http_ (httpc : Httpc.t) encoder ~path ~encode x : unit =
      Pbrt.Encoder.reset encoder;
      encode x encoder;
      let data = Pbrt.Encoder.to_string encoder in
      match Httpc.send httpc ~path ~decode:(fun _ -> ()) data with
      | Ok () -> ()
      | Error `Sysbreak ->
        Printf.eprintf "ctrl-c captured, stopping\n%!";
        Atomic.set stop true
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err

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

    let[@inline] guard_exn_ f =
      try f ()
      with e ->
        Printf.eprintf "opentelemetry-curl: uncaught exception: %s\n%!"
          (Printexc.to_string e)

    let emit_all_force (httpc : Httpc.t) encoder =
      let now = Mtime_clock.now () in
      ignore (emit_traces_maybe ~now ~force:true httpc encoder : bool);
      ignore (emit_logs_maybe ~now ~force:true httpc encoder : bool);
      ignore (emit_metrics_maybe ~now ~force:true httpc encoder : bool)
  end in
  if config.bg_threads > 0 then (
    (let m = Mutex.create () in
     Lock.set_mutex
       ~lock:(fun () -> Mutex.lock m)
       ~unlock:(fun () -> Mutex.unlock m));

    let m = Mutex.create () in
    let cond = Condition.create () in

    (* loop for the thread that processes events and sends them to collector *)
    let bg_thread () =
      let httpc = Httpc.create () in
      let encoder = Pbrt.Encoder.create () in
      while not @@ Atomic.get stop do
        let@ () = guard_exn_ in

        let now = Mtime_clock.now () in
        let do_metrics = emit_metrics_maybe ~now httpc encoder in
        let do_traces = emit_traces_maybe ~now httpc encoder in
        let do_logs = emit_logs_maybe ~now httpc encoder in
        if (not do_metrics) && (not do_traces) && not do_logs then
          (* wait for something to happen *)
          let@ () = with_mutex_ m in
          Condition.wait cond m
      done;
      (* flush remaining events once we exit *)
      let@ () = guard_exn_ in
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
      if Atomic.get needs_gc_metrics then sample_gc_metrics ();
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        !(!on_tick_cbs_);

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

    if config.ticker_thread then (
      (* thread that calls [tick()] regularly, to help enforce timeouts *)
      let tick_thread () =
        while not @@ Atomic.get stop do
          Thread.delay 0.5;
          tick ()
        done;
        wakeup ~all:true ()
      in

      start_bg_thread tick_thread
    );

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
        (* wakeup everyone *)
        with_mutex_ m (fun () -> Condition.broadcast cond)
    end in
    (module M)
  ) else (
    let httpc = Httpc.create () in
    let encoder = Pbrt.Encoder.create () in

    let module M = struct
      let push_trace e =
        let@ () = guard_exn_ in
        Batch.push' batch_traces e;
        let now = Mtime_clock.now () in
        ignore (emit_traces_maybe ~now httpc encoder : bool)

      let push_metrics e =
        let@ () = guard_exn_ in
        if Atomic.get needs_gc_metrics then sample_gc_metrics ();
        Batch.push' batch_metrics e;
        let now = Mtime_clock.now () in
        ignore (emit_metrics_maybe ~now httpc encoder : bool)

      let push_logs e =
        let@ () = guard_exn_ in
        Batch.push' batch_logs e;
        let now = Mtime_clock.now () in
        ignore (emit_logs_maybe ~now httpc encoder : bool)

      let set_on_tick_callbacks = set_on_tick_callbacks

      let tick () =
        if Atomic.get needs_gc_metrics then sample_gc_metrics ();
        let now = Mtime_clock.now () in
        ignore (emit_traces_maybe ~now httpc encoder : bool);
        ignore (emit_metrics_maybe ~now httpc encoder : bool);
        ignore (emit_logs_maybe ~now httpc encoder : bool);
        ()

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
          let@ () = Lock.with_lock in
          if !debug_ then
            Format.eprintf "send spans %a@."
              (Format.pp_print_list Trace.pp_resource_spans)
              l;
          push_trace l;
          ret ());
    }

  let last_sent_metrics = Atomic.make (Mtime_clock.now ())

  let timeout_sent_metrics = Mtime.Span.(5 * s)
  (* send metrics from time to time *)

  let signal_emit_gc_metrics () = Atomic.set needs_gc_metrics true

  let additional_metrics () : Metrics.resource_metrics list =
    (* add exporter metrics to the lot? *)
    let last_emit = Atomic.get last_sent_metrics in
    let now = Mtime_clock.now () in
    let add_own_metrics =
      let elapsed = Mtime.span last_emit now in
      Mtime.Span.compare elapsed timeout_sent_metrics > 0
    in

    if add_own_metrics then (
      let open OT.Metrics in
      Atomic.set last_sent_metrics now;
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
          if !debug_ then
            Format.eprintf "send metrics %a@."
              (Format.pp_print_list Metrics.pp_resource_metrics)
              m;

          let@ () = Lock.with_lock in

          let m = List.rev_append (additional_metrics ()) m in
          push_metrics m;
          ret ());
    }

  let send_logs : Logs.resource_logs list sender =
    {
      send =
        (fun m ~ret ->
          if !debug_ then
            Format.eprintf "send logs %a@."
              (Format.pp_print_list Logs.pp_resource_logs)
              m;

          let@ () = Lock.with_lock in
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
