(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module Config = Config
open Opentelemetry
open Opentelemetry_client
open Common_

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

external reraise : exn -> 'a = "%reraise"
(** This is equivalent to [Lwt.reraise]. We inline it here so we don't force to
    use Lwt's latest version *)

type error = Export_error.t

(* TODO: emit this in a metric in [tick()] if self tracing is enabled? *)
let n_errors = Atomic.make 0

let report_err_ = Export_error.report_err

(** HTTP client *)
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

  type t = Curl.t

  let create () : t = Ezcurl_core.make ()

  let cleanup self = Ezcurl_core.delete self

  (* send the content to the remote endpoint/path *)
  let send (self : t) ~url ~decode (bod : string) : ('a, error) result Lwt.t =
    let* r =
      let headers =
        ("Content-Type", "application/x-protobuf")
        :: ("Accept", "application/x-protobuf")
        :: Config.Env.get_headers ()
      in
      Ezcurl_lwt.post ~client:self ~headers ~params:[] ~url
        ~content:(`String bod) ()
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
      let err = Export_error.decode_invalid_http_response ~url ~code body in
      Lwt.return (Error err)
end

module Consumer_impl = struct
  module CNotifier = Opentelemetry_client_lwt.Notifier
  open Lwt.Syntax

  type state = {
    stop: bool Atomic.t;
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    config: Config.t;
    q: Any_resource.t Bounded_queue.t;
    notify: CNotifier.t;
  }

  let shutdown self =
    if not (Atomic.exchange self.stop true) then (
      CNotifier.trigger self.notify;
      CNotifier.delete self.notify
    )

  let send_http_ (self : state) (httpc : Httpc.t) ~url (data : string) :
      unit Lwt.t =
    let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
    match r with
    | Ok () -> Lwt.return ()
    | Error `Sysbreak ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      Atomic.set self.stop true;
      Lwt.return ()
    | Error err ->
      (* TODO: log error _via_ otel? *)
      Atomic.incr n_errors;
      report_err_ err;
      (* avoid crazy error loop *)
      Lwt_unix.sleep 3.

  let send_metrics_http (st : state) client ~encoder
      (l : Proto.Metrics.resource_metrics list) =
    let msg = Signal.Encode.metrics ~encoder l in
    Pbrt.Encoder.reset encoder;
    send_http_ st client msg ~url:st.config.url_metrics

  let send_traces_http st client ~encoder (l : Proto.Trace.resource_spans list)
      =
    let msg = Signal.Encode.traces ~encoder l in
    Pbrt.Encoder.reset encoder;
    send_http_ st client msg ~url:st.config.url_traces

  let send_logs_http st client ~encoder (l : Proto.Logs.resource_logs list) =
    let msg = Signal.Encode.logs ~encoder l in
    Pbrt.Encoder.reset encoder;
    send_http_ st client msg ~url:st.config.url_logs

  let tick (self : state) = CNotifier.trigger self.notify

  let start_worker (self : state) : unit =
    let client = Httpc.create () in
    let encoder = Pbrt.Encoder.create () in

    (* loop on [q] *)
    let rec loop () : unit Lwt.t =
      if Atomic.get self.stop then
        Lwt.return ()
      else
        let* () =
          match Bounded_queue.try_pop self.q with
          | `Closed ->
            shutdown self;
            Lwt.return ()
          | `Empty -> CNotifier.wait self.notify
          | `Item (R_logs logs) -> send_logs_http self client ~encoder logs
          | `Item (R_metrics ms) -> send_metrics_http self client ~encoder ms
          | `Item (R_spans spans) -> send_traces_http self client ~encoder spans
        in
        loop ()
    in

    Lwt.async (fun () ->
        Lwt.finalize loop (fun () ->
            Httpc.cleanup client;
            Lwt.return ()))

  let default_n_workers = 50

  let create_state ~stop ~config ~q () : state =
    let self =
      {
        stop;
        config;
        q;
        cleaned = Atomic.make false;
        notify = CNotifier.create ();
      }
    in

    (* start workers *)
    let n_workers =
      min 2
        (max 500
           (Option.value ~default:default_n_workers
              config.http_concurrency_level))
    in
    for _i = 1 to n_workers do
      start_worker self
    done;

    self

  let to_consumer (self : state) : Any_resource.t Consumer.t =
    let active () = not (Atomic.get self.stop) in
    let shutdown ~on_done =
      shutdown self;
      on_done ()
    in
    let tick () = tick self in
    { active; tick; shutdown }

  let consumer ~stop ~config () : Consumer.any_resource_builder =
    {
      start_consuming =
        (fun q ->
          let st = create_state ~stop ~config ~q () in
          to_consumer st);
    }
end

let create_consumer ?(stop = Atomic.make false) ?(config = Config.make ()) () =
  Consumer_impl.consumer ~stop ~config ()

let create_exporter ?stop ?(config = Config.make ()) () =
  let consumer = create_consumer ?stop ~config () in
  let bq =
    Bounded_queue_sync.create
      ~high_watermark:Bounded_queue.Defaults.high_watermark ()
  in
  Exporter_queued.create ~q:bq ~consumer ()
  |> Exporter_add_batching.add_batching ~config

let create_backend = create_exporter

let setup_ ?stop ?config () : unit =
  let exp = create_backend ?stop ?config () in
  Main_exporter.set exp;
  ()

let setup ?stop ?config ?(enable = true) () =
  if enable then setup_ ?stop ?config ()

let remove_backend () : unit Lwt.t =
  let done_fut, done_u = Lwt.wait () in
  Main_exporter.remove ~on_done:(fun () -> Lwt.wakeup_later done_u ()) ();
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
