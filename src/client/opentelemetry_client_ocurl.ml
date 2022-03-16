
(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

(* TODO *)

open Opentelemetry

let[@inline] (let@) f x = f x

let default_url = "http://localhost:4318"
let url = ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)
let set_url s = url := s

let lock_ : (unit -> unit) ref = ref ignore
let unlock_ : (unit -> unit) ref = ref ignore
let set_mutex ~lock ~unlock : unit =
  lock_ := lock;
  unlock_ := unlock

let[@inline] with_lock_ f =
  !lock_();
  Fun.protect ~finally:!unlock_ f

let _init = lazy (
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  at_exit Curl.global_cleanup;
)

module Backend() : Opentelemetry.Collector.BACKEND = struct
  let() = Lazy.force _init

  (* TODO: use Curl.Multi, etc. *)

  let encoder = Pbrt.Encoder.create()
  let buf_res = Buffer.create 256

  (* http client *)
  let curl : Curl.t = Curl.init ()

  let cleanup () = Curl.cleanup curl

  open Opentelemetry.Collector

  (* send the content to the remote endpoint/path *)
  let send_ ~path ~decode (bod:string) : ('a, int * Status.status) result =
    Curl.reset curl;
    Curl.set_url curl (!url ^ path);
    Curl.set_httppost curl [];
    Curl.set_httpheader curl ["content-type: application/x-protobuf"];
    (* write body *)
    Curl.set_readfunction curl
      begin
        let i = ref 0 in
        (fun n ->
           let len = min n (String.length bod - !i) in
           String.sub bod !i len)
      end;
    Buffer.clear buf_res;
    Curl.set_writefunction curl
      (fun s -> Buffer.add_string buf_res s; String.length s);
    match Curl.perform curl with
    | () ->
      let code = Curl.get_responsecode curl in
      let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
      if code >= 200 && code < 300 then (
        let res = decode dec in
        Ok res
      ) else (
        let status = Status.decode_status dec in
        Error (code, status)
      )
    | exception Curl.CurlException (_, code, msg) ->
      let status = Status.default_status
          ~code:(Int32.of_int code) ~message:(Bytes.unsafe_of_string msg) () in
      Error(code, status)

  let report_err_ code status =
    Format.eprintf "@[<2>opentelemetry: export failed with@ http code=%d@ status %a@]@."
      code Status.pp_status status

  let send_trace (tr:Trace_service.export_trace_service_request) : unit =
    let@() = with_lock_ in
    Pbrt.Encoder.reset encoder;
    Trace_service.encode_export_trace_service_request tr encoder;
    match
      send_ ~path:"/v1/traces" ~decode:(fun _ -> ())
        (Pbrt.Encoder.to_string encoder)
    with
    | Ok () -> ()
    | Error (code, status) -> report_err_ code status

  let send_metrics (m:Metrics_service.export_metrics_service_request) : unit =
    let@() = with_lock_ in
    Pbrt.Encoder.reset encoder;
    Metrics_service.encode_export_metrics_service_request m encoder;
    match
      send_ ~path:"/v1/metrics" ~decode:(fun _ -> ())
        (Pbrt.Encoder.to_string encoder);
    with
    | Ok () -> ()
    | Error (code, status) -> report_err_ code status
end

let setup_ () =
  let module B = Backend() in
  Opentelemetry.Collector.backend := Some (module B);
  B.cleanup

let setup() =
  let cleanup = setup_() in
  at_exit cleanup

let with_setup f =
  let cleanup = setup_() in
  Fun.protect ~finally:cleanup f
