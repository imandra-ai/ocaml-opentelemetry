open Eio.Std

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

(* Cross domain thread-safe storage for GC metrics gathered from different fibres.
   Ultimately appended to {!E_metrics}'s data on ticks. *)
module GC_metrics : sig
  val add : Proto.Metrics.resource_metrics -> unit
  val drain : unit -> Proto.Metrics.resource_metrics list
end
= struct
  (* Used to prevent data races across domains *)
  let mutex = Eio.Mutex.create ()
  let gc_metrics = ref []

  let add m = Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      gc_metrics := m :: !gc_metrics)

  let drain () = Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      let metrics = !gc_metrics in
      gc_metrics := [];
      metrics)
end

(* capture current GC metrics if {!needs_gc_metrics} is true,
   or it has been a long time since the last GC metrics collection,
   and push them into {!gc_metrics} for later collection *)
let sample_gc_metrics_if_needed () =
  let now = Mtime_clock.now () in
  let alarm = Atomic.compare_and_set needs_gc_metrics true false in
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
    GC_metrics.add l
  )

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

  val create :
    https:(Uri.t -> [ `Generic ] Eio.Net.stream_socket_ty r -> [> Eio.Flow.two_way_ty ] r) option ->
    [> [> `Generic ] Eio.Net.ty ] r ->
    t

  val send :
    t ->
    url:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result

  val cleanup : t -> unit
end = struct
  open Opentelemetry.Proto
  module Httpc = Cohttp_eio.Client

  type t = Httpc.t

  let create = Httpc.make

  let cleanup _self = ()

  (* send the content to the remote endpoint/path *)
  let send (client : t) ~url ~decode (bod : string) :
      ('a, error) result (* TODO: Does this need to return a promise? *)=
    Switch.run @@ fun sw ->
    let uri = Uri.of_string url in

    let open Cohttp in
    let headers = Header.(add_list (init ()) !headers) in
    let headers =
      Header.(add headers "Content-Type" "application/x-protobuf")
    in

    let body = Cohttp_eio.Body.of_string bod in
    let r =
      try
        let r = Httpc.post client ~sw  ~headers ~body uri in
        Ok r
      with e -> Error e
    in
    match r with
    | Error e ->
      let err =
        `Failure
          (spf "sending signals via http POST to %S\nfailed with:\n%s" url
             (Printexc.to_string e))
      in
      Error err
    | Ok (resp, body) ->
      let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
      let code = Response.status resp |> Code.code_of_status in
      if not (Code.is_error code) then (
        match decode with
        | `Ret x -> Ok x
        | `Dec f ->
          let dec = Pbrt.Decoder.of_string body in
          let r =
            try Ok (f dec)
            with e ->
              let bt = Printexc.get_backtrace () in
              Error
                (`Failure
                   (spf "decoding failed with:\n%s\n%s" (Printexc.to_string e)
                      bt))
          in
          r
      ) else (
        let dec = Pbrt.Decoder.of_string body in

        let r =
          try
            let status = Status.decode_pb_status dec in
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
                    url code (Printexc.to_string e) body bt))
        in
        r
      )
end

(** Batch of resources to be pushed later.

    This type is safe accross threads and domains. *)
module Batch : sig
  type 'a t

  val push' : 'a t -> 'a -> unit

  val pop_if_ready : ?force:bool -> now:Mtime.t -> 'a t -> 'a list option
  (** Is the batch ready to be emitted? If batching is disabled, this is true as
      soon as {!is_empty} is false. If a timeout is provided for this batch,
      then it will be ready if an element has been in it for at least the
      timeout.
      @param now passed to implement timeout *)

  val make : ?batch:int -> ?timeout:Mtime.span -> unit -> 'a t
  (** Create a new batch *)
end = struct

  module Q : sig
    type _ t
    val make : ?batch:int -> ?timeout:Mtime.span -> unit -> _ t
    val timeout : _ t -> Mtime.span option
    val size : _ t -> int
    val start : _ t -> Mtime.t
    val is_full : _ t -> bool

    (* Take all items queued for the batch, emptying the queue in the process *)
    val drain_q : 'a t -> 'a list

    val push : 'a t -> 'a -> bool
  end = struct
    let mutex = Eio.Mutex.create ()
    let w_mutex f = Eio.Mutex.use_rw ~protect:true mutex f
    let r_mutex f = Eio.Mutex.use_ro mutex f

    type 'a t = {
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
        size = 0;
        start = Mtime_clock.now ();
        q = [];
        batch;
        timeout;
        high_watermark;
      }

    let drain_q t = w_mutex (fun () ->
        let l = t.q in
        t.q <- [];
        t.size <- 0;
        l)

    (* Immutable fields that don't need a mutex for safe access *)
    let size t = t.size

    let is_full_ t : bool =
        match t.batch with
        | None -> t.size > 0
        | Some b -> t.size >= b

    let is_full t : bool = r_mutex (fun () -> is_full_ t)

    let push (t : _ t) x : bool = w_mutex (fun () ->
      if t.size >= t.high_watermark then (
        (* drop this to prevent queue from growing too fast *)
        Atomic.incr n_dropped;
        true
      ) else (
        if t.size = 0 && Option.is_some (t.timeout) then
          (* current batch starts now *)
          t.start <- Mtime_clock.now ();

        (* add to queue *)
        t.size <- 1 + t.size;
        t.q <- x :: t.q;
        let ready = is_full_ t in
        ready
      ))

    (* Access to mutable fields, requiring a mutex for safe access *)
    let timeout t = r_mutex (fun () -> t.timeout)
    let start t = r_mutex (fun () -> t.start)
  end

  type 'a t = 'a Q.t

  let make = Q.make

  let timeout_expired_ ~now self : bool =
    match Q.timeout self with
    | Some t ->
      let elapsed = Mtime.span now (Q.start self) in
      Mtime.Span.compare elapsed t >= 0
    | None -> false

  let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
    if Q.size self > 0 && (force || Q.is_full self || timeout_expired_ ~now self)
    then (
      let l = Q.drain_q self in
      assert (l <> []);
      Some l
    ) else
      None

  let push' self x = ignore (Q.push self x : bool)
end

(** An emitter. This is used by {!Backend} below to forward traces/metrics/…
    from the program to whatever collector client we have. *)
module type EMITTER = sig
  open Opentelemetry.Proto

  val push_trace : Trace.resource_spans list -> unit

  val push_metrics : Metrics.resource_metrics list -> unit

  val push_logs : Logs.resource_logs list -> unit

  val set_on_tick_callbacks : (unit -> unit) AList.t -> unit

  val tick : unit -> unit

  val cleanup : on_done:(unit -> unit) -> unit -> unit
end

(* Used by Switch.fail to signal when the collector is stopped. *)
exception Collector_stopped

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~stop ~(config : Config.t) ~(net:_ Eio.Net.t) () : (module EMITTER) =
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

    let on_tick_cbs_ = Atomic.make (AList.make ())

    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let send_http_ (httpc : Httpc.t) encoder ~url ~encode x : unit =
      Pbrt.Encoder.reset encoder;
      encode x encoder;
      let data = Pbrt.Encoder.to_string encoder in
      let r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
      match r with
      | Ok () -> ()
      | Error `Sysbreak ->
        Printf.eprintf "ctrl-c captured, stopping\n%!";
        Atomic.set stop true
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err;
        Eio_unix.sleep
          (* avoid crazy error loop *)
          3.

    let send_metrics_http curl encoder (l : Metrics.resource_metrics list list)
        =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Metrics_service.default_export_metrics_service_request
          ~resource_metrics:l ()
      in
      let url = config.Config.url_metrics in
      send_http_ curl encoder ~url
        ~encode:Metrics_service.encode_pb_export_metrics_service_request x

    let send_traces_http curl encoder (l : Trace.resource_spans list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Trace_service.default_export_trace_service_request ~resource_spans:l ()
      in
      let url = config.Config.url_traces in
      send_http_ curl encoder ~url
        ~encode:Trace_service.encode_pb_export_trace_service_request x

    let send_logs_http curl encoder (l : Logs.resource_logs list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Logs_service.default_export_logs_service_request ~resource_logs:l ()
      in
      let url = config.Config.url_logs in
      send_http_ curl encoder ~url
        ~encode:Logs_service.encode_pb_export_logs_service_request x

    (* emit metrics, if the batch is full or timeout lapsed *)
    let emit_metrics_maybe ~now ?force httpc encoder : bool =
      match Batch.pop_if_ready ?force ~now batch_metrics with
      | None -> false
      | Some collected_metrics ->
        (* We take a large but fixed number of gc_metrics so that can't be trapped
           forever waiting to exhaust a stream that is constantly being updated. *)
        let gc_metrics = GC_metrics.drain () in
        let batch = gc_metrics :: collected_metrics in
        let () = send_metrics_http httpc encoder batch in
        true

    let emit_traces_maybe ~now ?force httpc encoder : bool =
      match Batch.pop_if_ready ?force ~now batch_traces with
      | None -> false
      | Some l ->
        let () = send_traces_http httpc encoder l in
        true

    let emit_logs_maybe ~now ?force httpc encoder : bool  =
      match Batch.pop_if_ready ?force ~now batch_logs with
      | None -> false
      | Some l ->
        let () = send_logs_http httpc encoder l in
        true

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf
          "opentelemetry-curl: uncaught exception in %s: %s\n%s\n%!" where
          (Printexc.to_string e) bt

    let emit_all_force (httpc : Httpc.t) encoder : unit =
      let now = Mtime_clock.now () in
      Fiber.all [
        (fun () -> ignore @@ emit_logs_maybe ~now ~force:true httpc encoder);
        (fun () -> ignore @@ emit_metrics_maybe ~now ~force:true httpc encoder);
        (fun () -> ignore @@ emit_traces_maybe ~now ~force:true httpc encoder)
      ]

    let tick_common_ () =
      if !debug_ then Printf.eprintf "tick (from %d)\n%!" (tid ());
      sample_gc_metrics_if_needed ();
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        (AList.get @@ Atomic.get on_tick_cbs_);
      ()

    (* thread that calls [tick()] regularly, to help enforce timeouts *)
    let setup_ticker_thread ~tick ~sw () : unit =
      let rec tick_thread () =
        if Atomic.get stop then
          Switch.fail sw Collector_stopped
        else (
          let () = Eio_unix.sleep 0.5 in
          let () = tick () in
          tick_thread ()
        )
      in
      Fiber.fork ~sw tick_thread
  end in
  (* TODO: need https? *)
  let httpc = Httpc.create ~https:None net in
  let encoder = Pbrt.Encoder.create () in

  (* The entire module [M] shares the switch [sw]. This means that when the the
     switch is cancelled, every thread started within the module will be cancelled
     and have its resources disposed of. *)
  Switch.run @@ fun sw ->
  let module M = struct
    (* we make sure that this is thread-safe, even though we don't have a
       background thread. There can still be a ticker thread, and there
       can also be several user threads that produce spans and call
       the emit functions. *)

    let push_trace e =
      let@ () = guard_exn_ "push trace" in
      Batch.push' batch_traces e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw
        (fun () ->
          let (_ : bool) = emit_traces_maybe ~now httpc encoder in
          ())

    let push_metrics e =
      let@ () = guard_exn_ "push metrics" in
      sample_gc_metrics_if_needed ();
      Batch.push' batch_metrics e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw
        (fun () ->
          let (_ : bool) = emit_metrics_maybe ~now httpc encoder in
          ())

    let push_logs e =
      let@ () = guard_exn_ "push logs" in
      Batch.push' batch_logs e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw
        (fun () ->
          let (_ : bool) = emit_logs_maybe ~now httpc encoder in
          ())

    let set_on_tick_callbacks = set_on_tick_callbacks

    let tick_ () =
      tick_common_ ();
      sample_gc_metrics_if_needed ();
      let now = Mtime_clock.now () in
      Fiber.all [
        (fun () -> ignore @@ emit_logs_maybe ~now httpc encoder);
        (fun () -> ignore @@ emit_metrics_maybe ~now httpc encoder);
        (fun () -> ignore @@ emit_traces_maybe ~now httpc encoder)
      ]

    let () = setup_ticker_thread ~sw ~tick:tick_ ()

    (* if called in a blocking context: work in the background *)
    let tick () =
      Fiber.fork ~sw tick_

    let cleanup ~on_done () =
      if !debug_ then Printf.eprintf "opentelemetry: exiting…\n%!";
      Fiber.fork ~sw
        (fun () ->
          let () = emit_all_force httpc encoder in
          Httpc.cleanup httpc;
          on_done ())
  end in
  (module M : EMITTER)

(* Eio environment capabilities needed *)
(* TODO: not able to supply just this type for env due to weak :( *)
type 'a _env = 'a constraint 'a = <
    net : _ Eio.Net.t; (** Network access *)
  > as 'a

module Backend
    (Arg : sig
      val stop : bool Atomic.t

      (* Networking capability *)
      val env : Eio_unix.Stdenv.base

      val config : Config.t
    end)
    () : Opentelemetry.Collector.BACKEND = struct
  include (val mk_emitter ~stop:Arg.stop ~config:Arg.config ~net:Arg.env#net ())

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

let create_backend ?(stop = Atomic.make false) ?(config = Config.make ()) (env : Eio_unix.Stdenv.base) =
  debug_ := config.debug;

  let module B =
    Backend
      (struct
        let env = env
        let stop = stop
        let config = config
      end)
      ()
  in
  (module B : OT.Collector.BACKEND)

let setup_ ?stop ?config env : unit =
  let backend = create_backend ?stop ?config env in
  OT.Collector.set_backend backend;
  ()

let setup ?stop ?config ?(enable = true) env =
  if enable then setup_ ?stop ?config env

let remove_backend () =
  OT.Collector.remove_backend ~on_done:ignore ()

(* TODO: Work thru stop logic to make sure it is sensible *)
let with_setup ?stop ?(config = Config.make ()) ?(enable = true) env f =
  Switch.run begin fun sw ->
    if enable then (
      setup_ ?stop ~config env;
      Switch.on_release sw remove_backend
    );
    f ()
  end
