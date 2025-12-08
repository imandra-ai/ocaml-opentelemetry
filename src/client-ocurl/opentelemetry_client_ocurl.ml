(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module Config = Config
module OTELC = Opentelemetry_client
module OTEL = Opentelemetry
open Common_

let get_headers = Config.Env.get_headers

let set_headers = Config.Env.set_headers

let n_bytes_sent : int Atomic.t = Atomic.make 0

type error = OTELC.Export_error.t

open struct
  module Notifier = OTELC.Notifier_sync

  module IO : OTELC.Generic_io.S_WITH_CONCURRENCY with type 'a t = 'a = struct
    include OTELC.Generic_io.Direct_style

    let sleep_s = Thread.delay

    let[@inline] spawn f =
      ignore (OTELC.Util_thread.start_bg_thread f : Thread.t)
  end
end

module Httpc : OTELC.Generic_http_consumer.HTTPC with module IO = IO = struct
  module IO = IO

  type t = Curl.t

  let create () = Ezcurl.make ()

  let cleanup = Ezcurl.delete

  let send (self : t) ~url ~decode (bod : string) : ('a, error) result =
    let r =
      let headers =
        ("Content-Type", "application/x-protobuf")
        :: ("Accept", "application/x-protobuf")
        :: Config.Env.get_headers ()
      in
      Ezcurl.post ~client:self ~headers ~params:[] ~url ~content:(`String bod)
        ()
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
      Error err
    | Ok { code; body; _ } when code >= 200 && code < 300 ->
      (match decode with
      | `Ret x -> Ok x
      | `Dec f ->
        let dec = Pbrt.Decoder.of_string body in
        (try Ok (f dec)
         with e ->
           let bt = Printexc.get_backtrace () in
           Error
             (`Failure
                (spf "decoding failed with:\n%s\n%s" (Printexc.to_string e) bt))))
    | Ok { code; body; _ } ->
      let err =
        OTELC.Export_error.decode_invalid_http_response ~url ~code body
      in
      Error err
end

module Consumer_impl = OTELC.Generic_http_consumer.Make (IO) (Notifier) (Httpc)

let consumer ?(config = Config.make ()) () :
    Opentelemetry_client.Consumer.any_resource_builder =
  let n_workers = max 2 (min 32 config.bg_threads) in
  let ticker_task =
    if config.ticker_thread then
      Some (float config.ticker_interval_ms /. 1000.)
    else
      None
  in
  Consumer_impl.consumer ~override_n_workers:n_workers ~ticker_task
    ~config:config.common ()

let create_exporter ?(config = Config.make ()) () : OTEL.Exporter.t =
  let consumer = consumer ~config () in
  let bq =
    OTELC.Bounded_queue_sync.create
      ~high_watermark:OTELC.Bounded_queue.Defaults.high_watermark ()
  in

  OTELC.Exporter_queued.create ~q:bq ~consumer ()
  |> OTELC.Exporter_add_batching.add_batching ~config:config.common

let create_backend = create_exporter

let shutdown_and_wait (self : OTEL.Exporter.t) : unit =
  let open Opentelemetry_client in
  let sq = Sync_queue.create () in
  OTEL.Aswitch.on_turn_off (OTEL.Exporter.active self) (fun () ->
      Sync_queue.push sq ());
  OTEL.Exporter.shutdown self;
  Sync_queue.pop sq

let setup_ ?(config : Config.t = Config.make ()) () : OTEL.Exporter.t =
  let exporter = create_exporter ~config () in
  OTEL.Main_exporter.set exporter;

  OTELC.Self_trace.set_enabled config.common.self_trace;

  if config.ticker_thread then (
    (* at most a minute *)
    let sleep_ms = min 60_000 (max 2 config.ticker_interval_ms) in
    let active = OTEL.Exporter.active exporter in
    ignore
      (OTELC.Util_thread.setup_ticker_thread ~active ~sleep_ms exporter ()
        : Thread.t)
  );
  exporter

let remove_exporter () : unit =
  let open Opentelemetry_client in
  (* used to wait *)
  let sq = Sync_queue.create () in
  OTEL.Main_exporter.remove () ~on_done:(fun () -> Sync_queue.push sq ());
  Sync_queue.pop sq

let remove_backend = remove_exporter

let setup ?config ?(enable = true) () =
  if enable then ignore (setup_ ?config () : OTEL.Exporter.t)

let with_setup ?config ?(enable = true) () f =
  if enable then (
    let exp = setup_ ?config () in
    Fun.protect f ~finally:(fun () -> shutdown_and_wait exp)
  ) else
    f ()

let[@inline] n_bytes_sent () = Atomic.get n_bytes_sent
