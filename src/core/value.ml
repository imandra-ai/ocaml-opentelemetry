open Common_

type t =
  [ `Int of int
  | `String of string
  | `Bool of bool
  | `Float of float
  | `None
  ]
(** A value in a key/value attribute *)

let conv =
  let open Proto.Common in
  function
  | `Int i -> Some (Int_value (Int64.of_int i))
  | `String s -> Some (String_value s)
  | `Bool b -> Some (Bool_value b)
  | `Float f -> Some (Double_value f)
  | `None -> None
