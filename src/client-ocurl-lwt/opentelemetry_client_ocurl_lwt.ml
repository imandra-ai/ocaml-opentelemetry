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

type error = Export_error.t

open struct
  module IO = Opentelemetry_client_lwt.Io_lwt
end

(** HTTP client *)
module Httpc : Generic_http_consumer.HTTPC with module IO = IO = struct
  module IO = IO
  open Lwt.Syntax

  type t = Curl.t

  let create () : t = Ezcurl_core.make ()

  let cleanup self = Ezcurl_core.delete self

  (** send the content to the remote endpoint/path *)
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

module Consumer_impl =
  Generic_http_consumer.Make (IO) (Opentelemetry_client_lwt.Notifier_lwt)
    (Httpc)

let create_consumer ?(stop = Atomic.make false) ?(config = Config.make ()) () =
  Consumer_impl.consumer ~ticker_task:(Some 0.5) ~stop ~config ()

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
        Lwt.reraise exn)
  ) else
    f ()
