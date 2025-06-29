open Eio.Std

(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
module Config = Config
module Signal = Opentelemetry_client.Signal
module Batch = Opentelemetry_client.Batch
open Opentelemetry

let ( let@ ) = ( @@ )

let spf = Printf.sprintf

let tid () = Thread.id @@ Thread.self ()

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

(* Cross-domain, thread-safe storage for GC metrics gathered from different fibres. *)
module GC_metrics : sig
  val add : Proto.Metrics.resource_metrics -> unit

  val drain : unit -> Proto.Metrics.resource_metrics list
end = struct
  (* Used to prevent data races across domains *)
  let mutex = Eio.Mutex.create ()

  let gc_metrics = ref []

  let add m =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        gc_metrics := m :: !gc_metrics)

  let drain () =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
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

  val create : _ Eio.Net.t -> t

  val send :
    t ->
    url:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result
end = struct
  open Opentelemetry.Proto
  module Httpc = Cohttp_eio.Client

  type t = Httpc.t

  let authenticator =
    match Ca_certs.authenticator () with
    | Ok x -> x
    | Error (`Msg m) ->
      Fmt.failwith "Failed to create system store X509 authenticator: %s" m

  let https ~authenticator =
    let tls_config =
      match Tls.Config.client ~authenticator () with
      | Error (`Msg msg) -> failwith ("tls configuration problem: " ^ msg)
      | Ok tls_config -> tls_config
    in
    fun uri raw ->
      let host =
        Uri.host uri
        |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
      in
      Tls_eio.client_of_flow ?host tls_config raw

  let create net = Httpc.make ~https:(Some (https ~authenticator)) net

  (* send the content to the remote endpoint/path *)
  let send (client : t) ~url ~decode (bod : string) : ('a, error) result =
    Switch.run @@ fun sw ->
    let uri = Uri.of_string url in

    let open Cohttp in
    let headers = Header.(add_list (init ()) (Config.Env.get_headers ())) in
    let headers =
      Header.(add headers "Content-Type" "application/x-protobuf")
    in

    let body = Cohttp_eio.Body.of_string bod in
    let r =
      try
        let r = Httpc.post client ~sw ~headers ~body uri in
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
let mk_emitter ~sw ~stop ~(config : Config.t) ~(net : _ Eio.Net.t) () :
    (module EMITTER) =
  let open Proto in
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

    let send_http_ (httpc : Httpc.t) ~url data : unit =
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
        (* avoid crazy error loop *)
        Eio_unix.sleep 3.

    (* emit metrics, if the batch is full or timeout lapsed *)
    let emit_metrics_maybe ~now ?force client () =
      Batch.pop_if_ready ?force ~now batch_metrics
      |> Option.iter (fun collected_metrics ->
             let gc_metrics = GC_metrics.drain () in
             gc_metrics @ collected_metrics
             |> Signal.Encode.metrics
             |> send_http_ client ~url:config.url_metrics)

    let emit_traces_maybe ~now ?force client () =
      Batch.pop_if_ready ?force ~now batch_traces
      |> Option.iter (fun ts ->
             Signal.Encode.traces ts |> send_http_ client ~url:config.url_traces)

    let emit_logs_maybe ~now ?force client () =
      Batch.pop_if_ready ?force ~now batch_logs
      |> Option.iter (fun ls ->
             Signal.Encode.logs ls |> send_http_ client ~url:config.url_logs)

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf
          "opentelemetry-curl: uncaught exception in %s: %s\n%s\n%!" where
          (Printexc.to_string e) bt

    let emit_all_force (httpc : Httpc.t) : unit =
      let now = Mtime_clock.now () in
      Fiber.all
        [
          emit_logs_maybe ~now ~force:true httpc;
          emit_metrics_maybe ~now ~force:true httpc;
          emit_traces_maybe ~now ~force:true httpc;
        ]

    let tick_common_ () =
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
      ()

    (* thread that calls [tick()] regularly, to help enforce timeouts *)
    let ticker_fiber ~tick : unit -> [ `Stop_daemon ] =
      let rec loop () =
        if Atomic.get stop then
          `Stop_daemon
        else (
          tick ();
          Eio_unix.sleep 0.5;
          loop ()
        )
      in
      loop
  end in
  let httpc =
    (* Prime RNG state for TLS *)
    Mirage_crypto_rng_unix.use_default ();
    Httpc.create net
  in
  let module M = struct
    let push_to_batch b e =
      match Batch.push b e with
      | `Ok -> ()
      | `Dropped -> Atomic.incr n_errors

    let push_trace e =
      let@ () = guard_exn_ "push trace" in
      push_to_batch batch_traces e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw (emit_traces_maybe ~now httpc)

    let push_metrics e =
      let@ () = guard_exn_ "push metrics" in
      sample_gc_metrics_if_needed ();
      push_to_batch batch_metrics e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw (emit_metrics_maybe ~now httpc)

    let push_logs e =
      let@ () = guard_exn_ "push logs" in
      push_to_batch batch_logs e;
      let now = Mtime_clock.now () in
      Fiber.fork ~sw (emit_logs_maybe ~now httpc)

    let set_on_tick_callbacks = set_on_tick_callbacks

    let tick_ () =
      tick_common_ ();
      sample_gc_metrics_if_needed ();
      let now = Mtime_clock.now () in
      Fiber.all
        [
          emit_logs_maybe ~now httpc;
          emit_metrics_maybe ~now httpc;
          emit_traces_maybe ~now httpc;
        ]

    let () = Eio.Fiber.fork_daemon ~sw (ticker_fiber ~tick:tick_)

    let tick () = Fiber.fork ~sw tick_

    let cleanup ~on_done () =
      if Config.Env.get_debug () then
        Printf.eprintf "opentelemetry: exiting…\n%!";
      (* This must be in its own switch, because it MUST run even if the
         surrounding switch in the environment has been cancelled. *)
      Switch.run @@ fun sw ->
      Fiber.fork ~sw (fun () ->
          emit_all_force httpc;
          on_done ())
  end in
  (module M : EMITTER)

module Backend (Emitter : EMITTER) : Opentelemetry.Collector.BACKEND = struct
  include Emitter
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

let create_backend ~sw ?(stop = Atomic.make false) ?(config = Config.make ())
    (env : Eio_unix.Stdenv.base) : (module OT.Collector.BACKEND) =
  let module E = (val mk_emitter ~sw ~stop ~config ~net:env#net ()) in
  (module Backend (E))

let setup_ ~sw ?stop ?config env : unit =
  let backend = create_backend ~sw ?stop ?config env in
  OT.Collector.set_backend backend;
  ()

let setup ?stop ?config ?(enable = true) env =
  if enable then Switch.run @@ fun sw -> setup_ ~sw ?stop ?config env

let remove_backend () = OT.Collector.remove_backend ~on_done:ignore ()

let with_setup ?stop ?(config = Config.make ()) ?(enable = true) f env =
  (* NOTE: We must thread the switch [sw] through to all the forked threads in
     the Backend's Emitter, to ensure that we can wait on all of them to
     complete before before removing the backend during cleanup.  *)
  Switch.run (fun sw ->
      if enable then (
        setup_ ~sw ?stop ~config env;
        Switch.on_release sw remove_backend
      );
      f env)
