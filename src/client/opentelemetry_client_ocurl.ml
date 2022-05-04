
(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
open Opentelemetry
include Common_

let needs_gc_metrics = Atomic.make false

let gc_metrics = AList.make() (* side channel for GC, appended to {!E_metrics}'s data *)

(* capture current GC metrics and push them into {!gc_metrics} for later
   collection *)
let sample_gc_metrics () =
  Atomic.set needs_gc_metrics false;
  let l = OT.Metrics.make_resource_metrics
            ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
          @@ Opentelemetry.GC_metrics.get_metrics() in
  AList.add gc_metrics l

module Config = Config

let _init_curl = lazy (
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  at_exit Curl.global_cleanup;
)

type error = [
  | `Status of int * Opentelemetry.Proto.Status.status
  | `Failure of string
]

let n_errors = Atomic.make 0
let n_dropped = Atomic.make 0

let report_err_ = function
  | `Failure msg ->
    Format.eprintf "@[<2>opentelemetry: export failed: %s@]@." msg
  | `Status (code, status) ->
    Format.eprintf "@[<2>opentelemetry: export failed with@ http code=%d@ status %a@]@."
      code Proto.Status.pp_status status

module type CURL = sig
  val send : path:string -> decode:(Pbrt.Decoder.t -> 'a) -> string -> ('a, error) result
  val cleanup : unit -> unit
end

(* create a curl client *)
module Curl() : CURL = struct
  open Opentelemetry.Proto
  let() = Lazy.force _init_curl

  let buf_res = Buffer.create 256

  (* TODO: use Curl.Multi, etc. instead? *)

  (* http client *)
  let curl : Curl.t = Curl.init ()

  let cleanup () = Curl.cleanup curl

  (* TODO: use Curl multi *)

  (* send the content to the remote endpoint/path *)
  let send ~path ~decode (bod:string) : ('a, error) result =
    Curl.reset curl;
    if !debug_ then Curl.set_verbose curl true;
    Curl.set_url curl (!url ^ path);
    Curl.set_httppost curl [];
    let to_http_header (k, v) = Printf.sprintf "%s: %s" k v in
    let http_headers = List.map to_http_header !headers in
    Curl.set_httpheader curl ("Content-Type: application/x-protobuf" :: http_headers);
    (* write body *)
    Curl.set_post curl true;
    Curl.set_postfieldsize curl (String.length bod);
    Curl.set_readfunction curl
      begin
        let i = ref 0 in
        (fun n ->
           if !debug_ then Printf.eprintf "curl asks for %d bytes\n%!" n;
           let len = min n (String.length bod - !i) in
           let s = String.sub bod !i len in
           if !debug_ then Printf.eprintf "gave curl %d bytes\n%!" len;
           i := !i + len;
           s)
      end;
    (* read result's body *)
    Buffer.clear buf_res;
    Curl.set_writefunction curl
      (fun s -> Buffer.add_string buf_res s; String.length s);
    try
      match Curl.perform curl with
      | () ->
        let code = Curl.get_responsecode curl in
        if !debug_ then Printf.eprintf "result body: %S\n%!" (Buffer.contents buf_res);
        let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
        if code >= 200 && code < 300 then (
          let res = decode dec in
          Ok res
        ) else (
          let status = Status.decode_status dec in
          Error (`Status (code, status))
        )
      | exception Curl.CurlException (_, code, msg) ->
        let status = Status.default_status
            ~code:(Int32.of_int code) ~message:(Bytes.unsafe_of_string msg) () in
        Error(`Status (code, status))
    with e -> Error (`Failure (Printexc.to_string e))
end

module type PUSH = sig
  type elt
  val push : elt -> unit
  val is_empty : unit -> bool
  val is_big_enough : unit -> bool
  val pop_iter_all : (elt -> unit) -> unit
end

(** An emitter. This is used by {!Backend} below to forward traces/metrics/…
    from the program to whatever collector client we have. *)
module type EMITTER = sig
  open Opentelemetry.Proto

  val push_trace : Trace.resource_spans list -> unit
  val push_metrics : Metrics.resource_metrics list -> unit
  val set_on_tick_callbacks : (unit -> unit) list ref -> unit

  val tick : unit -> unit
  val cleanup : unit -> unit
end

type 'a push = (module PUSH with type elt = 'a)
type on_full_cb = (unit -> unit)

(* make a "push" object, along with a setter for a callback to call when
   it's ready to emit a batch *)
let mk_push (type a) ?batch () : (module PUSH with type elt = a) * (on_full_cb -> unit) =
  let on_full: on_full_cb ref = ref ignore in
  let push =
    match batch with
    | None ->
      let r = ref None in
      let module M = struct
        type elt = a
        let is_empty () = !r == None
        let is_big_enough () = !r != None
        let push x =
          r := Some x; !on_full()
        let pop_iter_all f = Option.iter f !r; r := None
      end in
      (module M : PUSH with type elt = a)

    | Some n ->
      let q = FQueue.create ~dummy:(Obj.magic 0) (3 * n) in
      let module M = struct
        type elt = a
        let is_empty () = FQueue.size q = 0
        let is_big_enough () = FQueue.size q >= n
        let push x =
          if not (FQueue.push q x) || FQueue.size q > n then (
            !on_full();
            if not (FQueue.push q x) then (
              Atomic.incr n_dropped; (* drop item *)
            )
          )
        let pop_iter_all f = FQueue.pop_iter_all q f
      end in
      (module M : PUSH with type elt = a)

  in
  push, ((:=) on_full)

(* start a thread in the background, running [f()] *)
let start_bg_thread (f: unit -> unit) : unit =
  let run() =
    (* block some signals: USR1 USR2 TERM PIPE ALARM STOP, see [$ kill -L] *)
    ignore (Thread.sigmask Unix.SIG_BLOCK [10; 12; 13; 14; 15; 19] : _ list);
    f()
  in
  ignore (Thread.create run () : Thread.t)

let l_is_empty = function [] -> true | _::_ -> false
let batch_is_empty = List.for_all l_is_empty

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~(config:Config.t) () : (module EMITTER) =
  let open Proto in

  let continue = ref true in

  let ((module E_trace) : Trace.resource_spans list push), on_trace_full =
    mk_push ?batch:config.batch_traces () in
  let ((module E_metrics) : Metrics.resource_metrics list push), on_metrics_full =
    mk_push ?batch:config.batch_metrics () in

  let encoder = Pbrt.Encoder.create() in

  let ((module C) as curl) = (module Curl() : CURL) in

  let on_tick_cbs_ = ref (ref []) in
  let set_on_tick_callbacks = (:=) on_tick_cbs_ in

  let send_metrics_http (l:Metrics.resource_metrics list list) =
    Pbrt.Encoder.reset encoder;
    let resource_metrics =
      List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    Metrics_service.encode_export_metrics_service_request
      (Metrics_service.default_export_metrics_service_request
         ~resource_metrics ())
      encoder;
    let data = Pbrt.Encoder.to_string encoder in
    begin match
        C.send ~path:"/v1/metrics" ~decode:(fun _ -> ())
          data
      with
      | Ok () -> ()
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err
    end;
  in

  let send_traces_http (l:Trace.resource_spans list list) =
    Pbrt.Encoder.reset encoder;
    let resource_spans =
      List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    Trace_service.encode_export_trace_service_request
      (Trace_service.default_export_trace_service_request ~resource_spans ())
      encoder;
    begin match
        C.send ~path:"/v1/traces" ~decode:(fun _ -> ())
          (Pbrt.Encoder.to_string encoder)
      with
      | Ok () -> ()
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err
    end;
  in

  let last_wakeup = Atomic.make (Mtime_clock.now()) in
  let timeout = Mtime.Span.(config.batch_timeout_ms * ms) in
  let batch_timeout() : bool =
    let elapsed = Mtime.span (Mtime_clock.now()) (Atomic.get last_wakeup) in
    Mtime.Span.compare elapsed timeout >= 0
  in

  let emit_metrics ?(force=false) () : bool =
    if force || (not force && E_metrics.is_big_enough ()) then (
      let batch = ref [AList.pop_all gc_metrics] in
      E_metrics.pop_iter_all (fun l -> batch := l :: !batch);
      let do_something = not (l_is_empty !batch) in
      if do_something then (
        send_metrics_http !batch;
        Atomic.set last_wakeup (Mtime_clock.now());
      );
      do_something
    ) else false
  in
  let emit_traces ?(force=false) () : bool =
    if force || (not force && E_trace.is_big_enough ()) then (
      let batch = ref [] in
      E_trace.pop_iter_all (fun l -> batch := l :: !batch);
      let do_something = not (l_is_empty !batch) in
      if do_something then (
        send_traces_http !batch;
        Atomic.set last_wakeup (Mtime_clock.now());
      );
      do_something
    ) else false
  in

  let[@inline] guard f =
    try f()
    with e ->
      Printf.eprintf "opentelemetry-curl: uncaught exception: %s\n%!"
        (Printexc.to_string e)
  in

  let emit_all_force () =
    let@ () = guard in
    ignore (emit_traces ~force:true () : bool);
    ignore (emit_metrics ~force:true () : bool);
  in


  if config.thread then (
    begin
      let m = Mutex.create() in
      set_mutex ~lock:(fun () -> Mutex.lock m) ~unlock:(fun () -> Mutex.unlock m);
    end;

    let ((module C) as curl) = (module Curl() : CURL) in

    let m = Mutex.create() in
    let cond = Condition.create() in

    (* loop for the thread that processes events and sends them to collector *)
    let bg_thread () =
      while !continue do
        let@ () = guard in
        let timeout = batch_timeout() in

        let do_metrics = emit_metrics ~force:timeout () in
        let do_traces = emit_traces ~force:timeout () in
        if not do_metrics && not do_traces then (
          (* wait *)
          let@ () = with_mutex_ m in
          Condition.wait cond m;
        )
      done;
      (* flush remaining events *)
      begin
        let@ () = guard in
        ignore (emit_traces ~force:true () : bool);
        ignore (emit_metrics ~force:true () : bool);
        C.cleanup();
      end
    in
    start_bg_thread bg_thread;

    let wakeup () =
      with_mutex_ m (fun () -> Condition.signal cond);
      Thread.yield()
    in

    (* wake up if a batch is full *)
    on_metrics_full wakeup;
    on_trace_full wakeup;

    let tick() =
      if Atomic.get needs_gc_metrics then sample_gc_metrics();
      List.iter
        (fun f ->
          try f()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n" (Printexc.to_string e))
        !(!on_tick_cbs_);
      if batch_timeout() then wakeup()
    in

    if config.ticker_thread then (
      (* thread that calls [tick()] regularly, to help enforce timeouts *)
      let tick_thread () =
        while true do
          Thread.delay 0.5;
          tick();
        done
      in

      start_bg_thread tick_thread;
    );

    let module M = struct
      let push_trace e =
        E_trace.push e;
        if batch_timeout() then wakeup()
      let push_metrics e =
        E_metrics.push e;
        if batch_timeout() then wakeup()
      let set_on_tick_callbacks = set_on_tick_callbacks
      let tick=tick
      let cleanup () =
        continue := false;
        with_mutex_ m (fun () -> Condition.broadcast cond)
    end in
    (module M)
  ) else (

    on_metrics_full (fun () ->
        if Atomic.get needs_gc_metrics then sample_gc_metrics();
        ignore (emit_metrics () : bool));
    on_trace_full (fun () ->
        ignore (emit_traces () : bool));

    let cleanup () =
      emit_all_force();
      C.cleanup();
    in

    let module M = struct
      let push_trace e =
        let@() = guard in
        E_trace.push e;
        if batch_timeout() then emit_all_force()

      let push_metrics e =
        let@() = guard in
        E_metrics.push e;
        if batch_timeout() then emit_all_force()

      let set_on_tick_callbacks = set_on_tick_callbacks

      let tick () =
        if Atomic.get needs_gc_metrics then sample_gc_metrics();
        if batch_timeout() then emit_all_force()

      let cleanup = cleanup
    end in
    (module M)
  )

module Backend(Arg : sig val config : Config.t end)()
  : Opentelemetry.Collector.BACKEND
= struct
  include Gen_ids.Make()

  include (val mk_emitter ~config:Arg.config ())

  open Opentelemetry.Proto
  open Opentelemetry.Collector

  let send_trace : Trace.resource_spans list sender = {
    send=fun l ~ret ->
      let@() = with_lock_ in
      if !debug_ then Format.eprintf "send spans %a@." (Format.pp_print_list Trace.pp_resource_spans) l;
      push_trace l;
      ret()
  }

  let last_sent_metrics = Atomic.make (Mtime_clock.now())
  let timeout_sent_metrics = Mtime.Span.(5 * s) (* send metrics from time to time *)

  let signal_emit_gc_metrics () = Atomic.set needs_gc_metrics true

  let additional_metrics () : Metrics.resource_metrics list =
    (* add exporter metrics to the lot? *)
    let last_emit = Atomic.get last_sent_metrics in
    let now = Mtime_clock.now() in
    let add_own_metrics =
      let elapsed = Mtime.span last_emit now in
      Mtime.Span.compare elapsed timeout_sent_metrics > 0
    in

    if add_own_metrics then (
      let open OT.Metrics in
      Atomic.set last_sent_metrics now;
      [make_resource_metrics [
        sum ~name:"otel-export.dropped" ~is_monotonic:true [
          int ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
            ~now:(Mtime.to_uint64_ns now) (Atomic.get n_dropped);
        ];
        sum ~name:"otel-export.errors" ~is_monotonic:true [
          int ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
            ~now:(Mtime.to_uint64_ns now) (Atomic.get n_errors);
        ];
      ]]
    ) else []

  let send_metrics : Metrics.resource_metrics list sender = {
    send=fun m ~ret ->
      let@() = with_lock_ in
      if !debug_ then Format.eprintf "send metrics %a@." (Format.pp_print_list Metrics.pp_resource_metrics) m;

      let m = List.rev_append (additional_metrics()) m in
      push_metrics m;
      ret()
  }
end

let setup_ ~(config:Config.t) () =
  debug_ := config.debug;
  let module B = Backend(struct let config=config end)() in
  Opentelemetry.Collector.set_backend (module B);
  B.cleanup

let setup ?(config=Config.make()) ?(enable=true) () =
  if enable then (
    let cleanup = setup_ ~config () in
    at_exit cleanup
  )

let with_setup ?(config=Config.make()) ?(enable=true) () f =
  if enable then (
    let cleanup = setup_ ~config () in
    Fun.protect ~finally:cleanup f
  ) else f()
