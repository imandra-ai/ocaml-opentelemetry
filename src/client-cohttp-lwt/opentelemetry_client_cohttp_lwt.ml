(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module Config = Config
open Opentelemetry_client
open Opentelemetry
open Common_

let set_headers = Config.Env.set_headers

let get_headers = Config.Env.get_headers

type error = Export_error.t

open struct
  module IO = Opentelemetry_client_lwt.Io_lwt
end

module Httpc : Generic_http_consumer.HTTPC with module IO = IO = struct
  module IO = IO
  open Opentelemetry.Proto
  open Lwt.Syntax
  module Httpc = Cohttp_lwt_unix.Client

  type t = unit

  let create () : t = ()

  let cleanup _self = ()

  (* send the content to the remote endpoint/path *)
  let send (_self : t) ~url ~decode (bod : string) : ('a, error) result Lwt.t =
    let uri = Uri.of_string url in

    let open Cohttp in
    let headers = Header.(add_list (init ()) (Config.Env.get_headers ())) in
    let headers =
      Header.(
        add_list headers
          [
            "Content-Type", "application/x-protobuf";
            "Accept", "application/x-protobuf";
          ])
    in

    let body = Cohttp_lwt.Body.of_string bod in

    let* r =
      try%lwt
        let+ r = Httpc.post ~headers ~body uri in
        Ok r
      with e -> Lwt.return @@ Error e
    in
    match r with
    | Error e ->
      let err =
        `Failure
          (spf "sending signals via http POST to %S\nfailed with:\n%s" url
             (Printexc.to_string e))
      in
      Lwt.return @@ Error err
    | Ok (resp, body) ->
      let* body = Cohttp_lwt.Body.to_string body in
      let code = Response.status resp |> Code.code_of_status in
      if not (Code.is_error code) then (
        match decode with
        | `Ret x -> Lwt.return @@ Ok x
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
          Lwt.return r
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
        Lwt.return r
      )
end

module Consumer_impl =
  Generic_http_consumer.Make (IO) (Opentelemetry_client_lwt.Notifier_lwt)
    (Httpc)

let create_consumer ?(config = Config.make ()) () =
  Consumer_impl.consumer ~ticker_task:(Some 0.5) ~config ()

let create_exporter ?(config = Config.make ()) () =
  let consumer = create_consumer ~config () in
  let bq =
    Bounded_queue_sync.create
      ~high_watermark:Bounded_queue.Defaults.high_watermark ()
  in
  Exporter_queued.create ~q:bq ~consumer ()
  |> Exporter_add_batching.add_batching ~config

let create_backend = create_exporter

let setup_ ?config () : unit =
  let backend = create_backend ?config () in
  Main_exporter.set backend;
  ()

let setup ?config ?(enable = true) () = if enable then setup_ ?config ()

let remove_exporter () : unit Lwt.t =
  let done_fut, done_u = Lwt.wait () in
  Main_exporter.remove ~on_done:(fun () -> Lwt.wakeup_later done_u ()) ();
  done_fut

let remove_backend = remove_exporter

let with_setup ?(config = Config.make ()) ?(enable = true) () f : _ Lwt.t =
  if enable then (
    let open Lwt.Syntax in
    setup_ ~config ();

    Lwt.catch
      (fun () ->
        let* res = f () in
        let+ () = remove_exporter () in
        res)
      (fun exn ->
        let* () = remove_exporter () in
        Lwt.reraise exn)
  ) else
    f ()
