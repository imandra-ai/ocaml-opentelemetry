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

let of_otel_opt (v : Proto.Common.any_value option) : t =
  match v with
  | Some (Int_value i) -> `Int (Int64.to_int i)
  | Some (String_value s) -> `String s
  | Some (Bool_value b) -> `Bool b
  | Some (Double_value f) -> `Float f
  | Some (Array_value _ | Kvlist_value _ | Bytes_value _) | None -> `None
