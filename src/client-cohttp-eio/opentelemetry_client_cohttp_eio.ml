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

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

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
  | `Status
      ( code,
        {
          Opentelemetry.Proto.Status.code = scode;
          message;
          details;
          _presence = _;
        } ) ->
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
  let send (client : t) ~url ~decode (body : string) : ('a, error) result =
    Switch.run @@ fun sw ->
    let uri = Uri.of_string url in

    let open Cohttp in
    let headers = Header.(add_list (init ()) (Config.Env.get_headers ())) in
    let headers =
      Header.(add headers "Content-Type" "application/x-protobuf")
    in

    let body = Cohttp_eio.Body.of_string body in
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

  val set_on_tick_callbacks : (unit -> unit) Alist.t -> unit

  val tick : unit -> unit

  val cleanup : on_done:(unit -> unit) -> unit -> unit
end

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~stop ~net (config : Config.t) : (module EMITTER) =
  (* local helpers *)
  let open struct
    let client =
      (* Prime RNG state for TLS *)
      Mirage_crypto_rng_unix.use_default ();
      Httpc.create net

    let send_http ~url data : unit =
      let r = Httpc.send client ~url ~decode:(`Ret ()) data in
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

    let timeout =
      if config.batch_timeout_ms > 0 then
        Some Mtime.Span.(config.batch_timeout_ms * ms)
      else
        None

    let batch_traces : Proto.Trace.resource_spans Batch.t =
      Batch.make ?batch:config.batch_traces ?timeout ()

    let batch_metrics : Proto.Metrics.resource_metrics Batch.t =
      Batch.make ?batch:config.batch_metrics ?timeout ()

    let batch_logs : Proto.Logs.resource_logs Batch.t =
      Batch.make ?batch:config.batch_logs ?timeout ()

    let push_to_batch b e =
      match Batch.push b e with
      | `Ok -> ()
      | `Dropped -> Atomic.incr n_errors

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf "opentelemetry-eio: uncaught exception in %s: %s\n%s\n%!"
          where (Printexc.to_string e) bt

    let push_traces x =
      let@ () = guard_exn_ "push trace" in
      push_to_batch batch_traces x

    let push_metrics x =
      let@ () = guard_exn_ "push metrics" in
      push_to_batch batch_metrics x

    let push_logs x =
      let@ () = guard_exn_ "push logs" in
      push_to_batch batch_logs x

    let maybe_emit (batch : 'a Batch.t) url (f : 'a list -> string) ~now ~force
        () : unit =
      Batch.pop_if_ready ~force ~now batch
      |> Option.iter (fun signals -> f signals |> send_http ~url)

    let emit_traces_maybe =
      maybe_emit batch_traces config.url_traces Signal.Encode.traces

    let emit_metrics_maybe =
      maybe_emit batch_metrics config.url_metrics (fun collected_metrics ->
          collected_metrics |> Signal.Encode.metrics)

    let emit_logs_maybe =
      maybe_emit batch_logs config.url_logs Signal.Encode.logs

    let emit_all ~force : unit =
      Switch.run @@ fun sw ->
      let now = Mtime_clock.now () in
      Fiber.fork ~sw @@ emit_logs_maybe ~now ~force;
      Fiber.fork ~sw @@ emit_metrics_maybe ~now ~force;
      Fiber.fork ~sw @@ emit_traces_maybe ~now ~force

    let on_tick_cbs_ = Atomic.make (Alist.make ())

    let run_tick_callbacks () =
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        (Alist.get @@ Atomic.get on_tick_cbs_)
  end in
  let module M = struct
    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let push_trace e = push_traces e

    let push_metrics e = push_metrics e

    let push_logs e = push_logs e

    let tick () =
      if Config.Env.get_debug () then
        Printf.eprintf "tick (from domain %d)\n%!" (Domain.self () :> int);
      run_tick_callbacks ();
      emit_all ~force:false

    let cleanup ~on_done () =
      if Config.Env.get_debug () then
        Printf.eprintf "opentelemetry: exiting…\n%!";
      Atomic.set stop true;
      run_tick_callbacks ();
      emit_all ~force:true;
      on_done ()
  end in
  (module M : EMITTER)

module Backend (Emitter : EMITTER) : Opentelemetry.Exporter.t = struct
  open Opentelemetry.Proto
  open Opentelemetry.Collector
  open Emitter

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

  let tick = Emitter.tick

  let cleanup = Emitter.cleanup

  let set_on_tick_callbacks = Emitter.set_on_tick_callbacks
end

let create_backend ~sw ?(stop = Atomic.make false) ?(config = Config.make ())
    env : (module OT.Collector.BACKEND) =
  let module E = (val mk_emitter ~stop ~net:env#net config) in
  let module B = Backend (E) in
  (* Run a background fiber to keep the backend ticking regularly.

     NOTE: This cannot be located inside the [Backend], because switches
     are not thread safe, and cannot be used accross domains, but the
     backend is accessed across domains. *)
  Eio.Fiber.fork ~sw (fun () ->
      while not @@ Atomic.get stop do
        Eio.Time.sleep env#clock 0.5;
        B.tick ()
      done);

  (module B)

let setup_ ~sw ?stop ?config env : unit =
  let backend = create_backend ?stop ?config ~sw env in
  OT.Collector.set_backend backend

let setup ?stop ?config ?(enable = true) ~sw env =
  if enable then setup_ ~sw ?stop ?config env

let remove_backend () = OT.Collector.remove_backend ~on_done:ignore ()

let with_setup ?stop ?config ?(enable = true) f env =
  if enable then
    Switch.run @@ fun sw ->
    snd
    @@ Fiber.pair
         (fun () -> setup_ ~sw ?stop ?config env)
         (fun () -> Fun.protect ~finally:(fun () -> remove_backend ()) f)
  else
    f ()
