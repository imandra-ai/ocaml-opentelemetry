(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
module Config = Config
module Signal = Opentelemetry_client.Signal
module Batch = Opentelemetry_client.Batch
open Opentelemetry
open Common_

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

external reraise : exn -> 'a = "%reraise"
(** This is equivalent to [Lwt.reraise]. We inline it here so we don't force to
    use Lwt's latest version *)

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

let gc_metrics = ref []
(* side channel for GC, appended to {!E_metrics}'s data *)

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
    gc_metrics := l :: !gc_metrics
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

  val create : unit -> t

  val send :
    t ->
    url:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result Lwt.t

  val cleanup : t -> unit
end = struct
  open Opentelemetry.Proto
  open Lwt.Syntax

  type t = unit

  let create () : t = ()

  let cleanup _self = ()

  (* send the content to the remote endpoint/path *)
  let send (_self : t) ~url ~decode (bod : string) : ('a, error) result Lwt.t =
    let* r =
      let headers =
        ("Content-Type", "application/x-protobuf")
        :: ("Accept", "application/x-protobuf")
        :: Config.Env.get_headers ()
      in
      Ezcurl_lwt.post ~headers ~params:[] ~url ~content:(`String bod) ()
    in
    match r with
    | Error (code, msg) ->
      let err =
        `Failure
          (spf
             "sending signals via http POST failed:\n\
             \  %s\n\
             \  curl code: %s\n\
             \  url: %s\n\
              %!"
             msg (Curl.strerror code) url)
      in
      Lwt.return @@ Error err
    | Ok { code; body; _ } when code >= 200 && code < 300 ->
      (match decode with
      | `Ret x -> Lwt.return @@ Ok x
      | `Dec f ->
        let dec = Pbrt.Decoder.of_string body in
        let r =
          try Ok (f dec)
          with e ->
            let bt = Printexc.get_backtrace () in
            Error
              (`Failure
                 (spf "decoding failed with:\n%s\n%s" (Printexc.to_string e) bt))
        in
        Lwt.return r)
    | Ok { code; body; _ } ->
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
      Lwt.return r
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

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~stop ~(config : Config.t) () : (module EMITTER) =
  let open Proto in
  let open Lwt.Syntax in
  (* local helpers *)
  let open struct
    let timeout =
      if config.batch_timeout_ms > 0 then
        Some Mtime.Span.(config.batch_timeout_ms * ms)
      else
        None

    let batch_traces : Trace.resource_spans Batch.t =
      Batch.make ?batch:config.batch_traces ?timeout ()

    let batch_metrics : Metrics.resource_metrics Batch.t =
      Batch.make ?batch:config.batch_metrics ?timeout ()

    let batch_logs : Logs.resource_logs Batch.t =
      Batch.make ?batch:config.batch_logs ?timeout ()

    let on_tick_cbs_ = Atomic.make (AList.make ())

    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let send_http_ (httpc : Httpc.t) ~url data : unit Lwt.t =
      let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
      match r with
      | Ok () -> Lwt.return ()
      | Error `Sysbreak ->
        Printf.eprintf "ctrl-c captured, stopping\n%!";
        Atomic.set stop true;
        Lwt.return ()
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err;
        (* avoid crazy error loop *)
        Lwt_unix.sleep 3.

    let send_metrics_http client (l : Metrics.resource_metrics list) =
      Signal.Encode.metrics l |> send_http_ client ~url:config.url_metrics

    let send_traces_http client (l : Trace.resource_spans list) =
      Signal.Encode.traces l |> send_http_ client ~url:config.url_traces

    let send_logs_http client (l : Logs.resource_logs list) =
      Signal.Encode.logs l |> send_http_ client ~url:config.url_logs

    (* emit metrics, if the batch is full or timeout lapsed *)
    let emit_metrics_maybe ~now ?force httpc : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_metrics with
      | None -> Lwt.return false
      | Some l ->
        let batch = !gc_metrics @ l in
        gc_metrics := [];
        let+ () = send_metrics_http httpc batch in
        true

    let emit_traces_maybe ~now ?force httpc : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_traces with
      | None -> Lwt.return false
      | Some l ->
        let+ () = send_traces_http httpc l in
        true

    let emit_logs_maybe ~now ?force httpc : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_logs with
      | None -> Lwt.return false
      | Some l ->
        let+ () = send_logs_http httpc l in
        true

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf
          "opentelemetry-ocurl-lwt: uncaught exception in %s: %s\n%s\n%!" where
          (Printexc.to_string e) bt

    let emit_all_force (httpc : Httpc.t) : unit Lwt.t =
      let now = Mtime_clock.now () in
      let+ (_ : bool) = emit_traces_maybe ~now ~force:true httpc
      and+ (_ : bool) = emit_logs_maybe ~now ~force:true httpc
      and+ (_ : bool) = emit_metrics_maybe ~now ~force:true httpc in
      ()

    (* thread that calls [tick()] regularly, to help enforce timeouts *)
    let setup_ticker_thread ~tick ~finally () =
      let rec tick_thread () =
        if Atomic.get stop then (
          finally ();
          Lwt.return ()
        ) else
          let* () = Lwt_unix.sleep 0.5 in
          let* () = tick () in
          tick_thread ()
      in
      Lwt.async tick_thread
  end in
  let httpc = Httpc.create () in

  let module M = struct
    (* we make sure that this is thread-safe, even though we don't have a
       background thread. There can still be a ticker thread, and there
       can also be several user threads that produce spans and call
       the emit functions. *)

    let push_to_batch b e =
      match Batch.push b e with
      | `Ok -> ()
      | `Dropped -> Atomic.incr n_dropped

    let push_trace e =
      let@ () = guard_exn_ "push trace" in
      push_to_batch batch_traces e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_traces_maybe ~now httpc in
          ())

    let push_metrics e =
      let@ () = guard_exn_ "push metrics" in
      sample_gc_metrics_if_needed ();
      push_to_batch batch_metrics e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_metrics_maybe ~now httpc in
          ())

    let push_logs e =
      let@ () = guard_exn_ "push logs" in
      push_to_batch batch_logs e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_logs_maybe ~now httpc in
          ())

    let set_on_tick_callbacks = set_on_tick_callbacks

    let tick_ () =
      if Config.Env.get_debug () then
        Printf.eprintf "tick (from %d)\n%!" (tid ());
      sample_gc_metrics_if_needed ();
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        (AList.get @@ Atomic.get on_tick_cbs_);
      let now = Mtime_clock.now () in
      let+ (_ : bool) = emit_traces_maybe ~now httpc
      and+ (_ : bool) = emit_logs_maybe ~now httpc
      and+ (_ : bool) = emit_metrics_maybe ~now httpc in
      ()

    let () = setup_ticker_thread ~tick:tick_ ~finally:ignore ()

    (* if called in a blocking context: work in the background *)
    let tick () = Lwt.async tick_

    let cleanup ~on_done () =
      if Config.Env.get_debug () then
        Printf.eprintf "opentelemetry: exiting…\n%!";
      Lwt.async (fun () ->
          let* () = emit_all_force httpc in
          Httpc.cleanup httpc;
          on_done ();
          Lwt.return ())
  end in
  (module M)

module Backend
    (Arg : sig
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
          (if Config.Env.get_debug () then
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
    if Config.Env.get_debug () then
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
          (if Config.Env.get_debug () then
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
          (if Config.Env.get_debug () then
             let@ () = Lock.with_lock in
             Format.eprintf "send logs %a@."
               (Format.pp_print_list Logs.pp_resource_logs)
               m);

          push_logs m;
          ret ());
    }
end

let create_backend ?(stop = Atomic.make false) ?(config = Config.make ()) () =
  let module B =
    Backend
      (struct
        let stop = stop

        let config = config
      end)
      ()
  in
  (module B : OT.Collector.BACKEND)

let setup_ ?stop ?config () : unit =
  let backend = create_backend ?stop ?config () in
  OT.Collector.set_backend backend;
  ()

let setup ?stop ?config ?(enable = true) () =
  if enable then setup_ ?stop ?config ()

let remove_backend () : unit Lwt.t =
  let done_fut, done_u = Lwt.wait () in
  OT.Collector.remove_backend ~on_done:(fun () -> Lwt.wakeup_later done_u ()) ();
  done_fut

let with_setup ?stop ?(config = Config.make ()) ?(enable = true) () f : _ Lwt.t
    =
  if enable then (
    let open Lwt.Syntax in
    setup_ ?stop ~config ();

    Lwt.catch
      (fun () ->
        let* res = f () in
        let+ () = remove_backend () in
        res)
      (fun exn ->
        let* () = remove_backend () in
        reraise exn)
  ) else
    f ()
