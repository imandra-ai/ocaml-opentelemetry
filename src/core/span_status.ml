open Common_
open Proto.Trace

type t = Proto.Trace.status = private {
  mutable _presence: Pbrt.Bitfield.t;
  mutable message: string;
  mutable code: status_status_code;
}

type code = status_status_code =
  | Status_code_unset
  | Status_code_ok
  | Status_code_error

let[@inline] make ~message ~code : t = make_status ~message ~code ()
