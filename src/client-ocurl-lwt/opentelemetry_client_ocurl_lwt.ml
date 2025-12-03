(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
open Opentelemetry
open Opentelemetry_util
open Opentelemetry_client
open Common_

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

external reraise : exn -> 'a = "%reraise"
(** This is equivalent to [Lwt.reraise]. We inline it here so we don't force to
    use Lwt's latest version *)

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

  (* FIXME: absolutely need some rate limiting somewhere, ideally as early
     as possible so we can measure how many resources we drop *)

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

module Exporter_impl = struct
  open Lwt.Syntax

  let[@inline] push_to_batch b e =
    if e <> [] then (
      match Batch.push b e with
      | `Ok -> ()
      | `Dropped -> Atomic.incr n_dropped
    )

  type state = {
    stop: bool Atomic.t;
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    config: Config.t;
    encoder_pool: Pbrt.Encoder.t Rpool.t;
    traces: Proto.Trace.span Batch.t;
    logs: Proto.Logs.log_record Batch.t;
    metrics: Proto.Metrics.metric Batch.t;
  }

  let send_http_ (st : state) (httpc : Httpc.t) ~url data : unit Lwt.t =
    let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
    match r with
    | Ok () -> Lwt.return ()
    | Error `Sysbreak ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      Atomic.set st.stop true;
      Lwt.return ()
    | Error err ->
      (* TODO: log error _via_ otel? *)
      Atomic.incr n_errors;
      report_err_ err;
      (* avoid crazy error loop *)
      Lwt_unix.sleep 3.

  let send_metrics_http (st : state) client
      (l : Proto.Metrics.resource_metrics list) =
    let msg =
      let@ encoder = Rpool.with_resource st.encoder_pool in
      Signal.Encode.metrics ~encoder l
    in
    send_http_ st client msg ~url:st.config.url_metrics

  let send_traces_http st client (l : Proto.Trace.resource_spans list) =
    let msg =
      let@ encoder = Rpool.with_resource st.encoder_pool in
      Signal.Encode.traces ~encoder l
    in
    send_http_ st client msg ~url:st.config.url_traces

  let send_logs_http st client (l : Proto.Logs.resource_logs list) =
    let msg =
      let@ encoder = Rpool.with_resource st.encoder_pool in
      Signal.Encode.logs ~encoder l
    in
    send_http_ st client msg ~url:st.config.url_logs

  (* emit metrics, if the batch is full or timeout lapsed *)
  let emit_metrics_maybe (st : state) ~now ?force httpc : bool Lwt.t =
    match Batch.pop_if_ready ?force ~now st.metrics with
    | None -> Lwt.return false
    | Some l ->
      let res = Util_resources.make_resource_metrics l in
      let+ () = send_metrics_http st httpc [ res ] in
      true

  let emit_traces_maybe st ~now ?force httpc : bool Lwt.t =
    match Batch.pop_if_ready ?force ~now st.traces with
    | None -> Lwt.return false
    | Some l ->
      let res = Util_resources.make_resource_spans l in
      let+ () = send_traces_http st httpc [ res ] in
      true

  let emit_logs_maybe st ~now ?force httpc : bool Lwt.t =
    match Batch.pop_if_ready ?force ~now st.logs with
    | None -> Lwt.return false
    | Some l ->
      let res = Util_resources.make_resource_logs l in
      let+ () = send_logs_http st httpc [ res ] in
      true

  let emit_all_force st (httpc : Httpc.t) : unit Lwt.t =
    let now = Mtime_clock.now () in
    let+ (_ : bool) = emit_traces_maybe st ~now ~force:true httpc
    and+ (_ : bool) = emit_logs_maybe st ~now ~force:true httpc
    and+ (_ : bool) = emit_metrics_maybe st ~now ~force:true httpc in
    ()

  let[@inline] guard_exn_ where f =
    try f ()
    with e ->
      let bt = Printexc.get_backtrace () in
      Printf.eprintf
        "opentelemetry-ocurl-lwt: uncaught exception in %s: %s\n%s\n%!" where
        (Printexc.to_string e) bt

  (* Lwt task that calls [tick()] regularly, to help enforce timeouts *)
  let setup_ticker_ st ~tick ~finally () =
    let rec tick_loop () =
      if Atomic.get st.stop then (
        finally ();
        Lwt.return ()
      ) else
        let* () = Lwt_unix.sleep 0.5 in
        let* () = tick () in
        tick_loop ()
    in
    Lwt.async tick_loop

  (* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
  let create ~stop ~(config : Config.t) () : OT.Exporter.t =
    let open Proto in
    let encoder_pool =
      Rpool.create
        ~mk_item:(fun () -> Pbrt.Encoder.create ~size:1024 ())
        ~max_size:32 ~clear:Pbrt.Encoder.reset ()
    in

    (* local helpers *)
    let timeout =
      if config.batch_timeout_ms > 0 then
        Some Mtime.Span.(config.batch_timeout_ms * ms)
      else
        None
    in

    let st =
      {
        stop;
        config;
        cleaned = Atomic.make false;
        encoder_pool;
        traces = Batch.make ?batch:config.batch_traces ?timeout ();
        metrics = Batch.make ?batch:config.batch_metrics ?timeout ();
        logs = Batch.make ?batch:config.batch_logs ?timeout ();
      }
    in
    let httpc = Httpc.create () in
    let ticker = Cb_set.create () in

    let tick_ () =
      if Config.Env.get_debug () then
        Printf.eprintf "tick (from %d)\n%!" (Thread.id @@ Thread.self ());
      Cb_set.trigger ticker;
      let now = Mtime_clock.now () in
      let+ (_ : bool) = emit_traces_maybe st ~now httpc
      and+ (_ : bool) = emit_logs_maybe st ~now httpc
      and+ (_ : bool) = emit_metrics_maybe st ~now httpc in
      ()
    in

    setup_ticker_ st ~tick:tick_ ~finally:ignore ();

    (* we make sure that this is thread-safe, even though we don't have a
       background thread. There can still be a ticker thread, and there
       can also be several user threads that produce spans and call
       the emit functions. *)
    object
      method send_trace e =
        let@ () = guard_exn_ "push trace" in
        push_to_batch st.traces e;
        let now = Mtime_clock.now () in
        Lwt.async (fun () ->
            let+ (_ : bool) = emit_traces_maybe st ~now httpc in
            ())

      method send_metrics e =
        let@ () = guard_exn_ "push metrics" in
        push_to_batch st.metrics e;
        let now = Mtime_clock.now () in
        Lwt.async (fun () ->
            let+ (_ : bool) = emit_metrics_maybe st ~now httpc in
            ())

      method send_logs e =
        let@ () = guard_exn_ "push logs" in
        push_to_batch st.logs e;
        let now = Mtime_clock.now () in
        Lwt.async (fun () ->
            let+ (_ : bool) = emit_logs_maybe st ~now httpc in
            ())

      method add_on_tick_callback f = Cb_set.register ticker f

      (* if called in a blocking context: work in the background *)
      method tick () = Lwt.async tick_

      method cleanup ~on_done () =
        if Config.Env.get_debug () then
          Printf.eprintf "opentelemetry: exiting…\n%!";
        Lwt.async (fun () ->
            let* () = emit_all_force st httpc in
            Httpc.cleanup httpc;
            on_done ();
            Lwt.return ())
    end
end

let create_backend ?(stop = Atomic.make false) ?(config = Config.make ()) () =
  Exporter_impl.create ~stop ~config ()

let setup_ ?stop ?config () : unit =
  let exp = create_backend ?stop ?config () in
  OT.Exporter.Main_exporter.set exp;
  ()

let setup ?stop ?config ?(enable = true) () =
  if enable then setup_ ?stop ?config ()

let remove_backend () : unit Lwt.t =
  let done_fut, done_u = Lwt.wait () in
  OT.Exporter.Main_exporter.remove
    ~on_done:(fun () -> Lwt.wakeup_later done_u ())
    ();
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
