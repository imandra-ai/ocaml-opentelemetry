open Common_

type t = bytes

let[@inline] to_bytes self = self

let dummy : t = Bytes.make 8 '\x00'

let create () : t =
  let b = Rand_bytes.rand_bytes_8 () in
  assert (Bytes.length b = 8);
  (* make sure the identifier is not all 0, which is a dummy identifier. *)
  Bytes.set b 0 (Char.unsafe_chr (Char.code (Bytes.get b 0) lor 1));
  b

let is_valid = Util_bytes_.bytes_non_zero

let[@inline] of_bytes b =
  if Bytes.length b = 8 then
    b
  else
    invalid_arg "span IDs must be 8 bytes in length"

let to_hex = Util_bytes_.bytes_to_hex

let to_hex_into = Util_bytes_.bytes_to_hex_into

let[@inline] of_hex s = of_bytes (Util_bytes_.bytes_of_hex s)

let[@inline] of_hex_substring s off =
  of_bytes (Util_bytes_.bytes_of_hex_substring s off 16)

let pp fmt t = Format.fprintf fmt "%s" (to_hex t)
