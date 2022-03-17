
(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

(* TODO *)

open Opentelemetry

let[@inline] (let@) f x = f x

let debug_ = ref (try bool_of_string @@ Sys.getenv "DEBUG" with _ -> false)

let default_url = "http://localhost:4318"
let url = ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)
let get_url () = !url
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

  let rand_ = Random.State.make_self_init()

  (* http client *)
  let curl : Curl.t = Curl.init ()

  let cleanup () = Curl.cleanup curl

  open Opentelemetry.Proto
  open Opentelemetry.Collector

  type error = [
    | `Status of int * Status.status
    | `Failure of string
  ]

  (* send the content to the remote endpoint/path *)
  let send_ ~path ~decode (bod:string) : ('a, error) result =
    Curl.reset curl;
    Curl.set_verbose curl true;
    Curl.set_url curl (!url ^ path);
    Curl.set_httppost curl [];
    Curl.set_httpheader curl ["Content-Type: application/x-protobuf"];
    (* write body *)
    Curl.set_post curl true;
    Curl.set_postfieldsize curl (String.length bod);
    Curl.set_readfunction curl
      begin
        let i = ref 0 in
        (fun n ->
           if !debug_ then Printf.eprintf "curl asks for %d bytes\n%!" n;
           let len = min n (String.length bod - !i) in
           let s = String.sub bod !i len in
           if !debug_ then Printf.eprintf "gave curl %d bytes\n%!" len;
           i := !i + len;
           s)
      end;
    (* read result's body *)
    Buffer.clear buf_res;
    Curl.set_writefunction curl
      (fun s -> Buffer.add_string buf_res s; String.length s);
    try
      match Curl.perform curl with
      | () ->
        let code = Curl.get_responsecode curl in
        if !debug_ then Printf.eprintf "result body: %S\n%!" (Buffer.contents buf_res);
        let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
        if code >= 200 && code < 300 then (
          let res = decode dec in
          Ok res
        ) else (
          let status = Status.decode_status dec in
          Error (`Status (code, status))
        )
      | exception Curl.CurlException (_, code, msg) ->
        let status = Status.default_status
            ~code:(Int32.of_int code) ~message:(Bytes.unsafe_of_string msg) () in
        Error(`Status (code, status))
    with e -> Error (`Failure (Printexc.to_string e))

  let report_err_ = function
    | `Failure msg ->
      Format.eprintf "@[<2>opentelemetry: export failed: %s@]@." msg
    | `Status (code, status) ->
      Format.eprintf "@[<2>opentelemetry: export failed with@ http code=%d@ status %a@]@."
        code Status.pp_status status

  let send_trace (tr:Trace_service.export_trace_service_request) : unit =
    let@() = with_lock_ in
    if !debug_ then Format.eprintf "send trace %a@." Trace_service.pp_export_trace_service_request tr;
    Pbrt.Encoder.reset encoder;
    Trace_service.encode_export_trace_service_request tr encoder;
    match
      send_ ~path:"/v1/traces" ~decode:(fun _ -> ())
        (Pbrt.Encoder.to_string encoder)
    with
    | Ok () -> ()
    | Error err -> report_err_ err

  let send_metrics (m:Metrics_service.export_metrics_service_request) : unit =
    let@() = with_lock_ in
    if !debug_ then Format.eprintf "send metrics %a@." Metrics_service.pp_export_metrics_service_request m;
    Pbrt.Encoder.reset encoder;
    Metrics_service.encode_export_metrics_service_request m encoder;
    match
      send_ ~path:"/v1/metrics" ~decode:(fun _ -> ())
        (Pbrt.Encoder.to_string encoder);
    with
    | Ok () -> ()
    | Error err -> report_err_ err

  let rand_bytes_8 () : bytes =
    let@() = with_lock_ in
    let b = Bytes.create 8 in
    for i=0 to 1 do
      let r = Random.State.bits rand_ in (* 30 bits, of which we use 24 *)
      Bytes.set b (i*3) (Char.chr (r land 0xff));
      Bytes.set b (i*3+1) (Char.chr (r lsr 8 land 0xff));
      Bytes.set b (i*3+2) (Char.chr (r lsr 16 land 0xff));
    done;
    let r = Random.State.bits rand_ in
    Bytes.set b 6 (Char.chr (r land 0xff));
    Bytes.set b 7 (Char.chr (r lsr 8 land 0xff));
    b

  let rand_bytes_16 () : bytes =
    let@() = with_lock_ in
    let b = Bytes.create 16 in
    for i=0 to 4 do
      let r = Random.State.bits rand_ in (* 30 bits, of which we use 24 *)
      Bytes.set b (i*3) (Char.chr (r land 0xff));
      Bytes.set b (i*3+1) (Char.chr (r lsr 8 land 0xff));
      Bytes.set b (i*3+2) (Char.chr (r lsr 16 land 0xff));
    done;
    let r = Random.State.bits rand_ in
    Bytes.set b 15 (Char.chr (r land 0xff)); (* last byte *)
    b
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
