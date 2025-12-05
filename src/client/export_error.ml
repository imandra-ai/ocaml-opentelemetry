open Common_

type t =
  [ `Status of int * Opentelemetry.Proto.Status.status
  | `Failure of string
  | `Sysbreak
  ]

let str_to_hex (s : string) : string =
  Opentelemetry_util.Util_bytes_.bytes_to_hex (Bytes.unsafe_of_string s)

(** Report the error on stderr. *)
let report_err : t -> unit = function
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

let decode_invalid_http_response ~code ~url (body : string) : t =
  try
    let dec = Pbrt.Decoder.of_string body in
    let status = Opentelemetry.Proto.Status.decode_pb_status dec in
    `Status (code, status)
  with e ->
    let bt = Printexc.get_backtrace () in
    `Failure
      (Printf.sprintf
         "httpc: decoding of status (url=%S, code=%d) failed with:\n\
          %s\n\
          HTTP body: %s\n\
          %s"
         url code (Printexc.to_string e) (str_to_hex body) bt)
