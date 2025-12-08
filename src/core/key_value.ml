open Common_

type t = string * Value.t

let conv (k, v) =
  let open Proto.Common in
  let value = Value.conv v in
  make_key_value ~key:k ?value ()

let of_otel (kv : Proto.Common.key_value) : t =
  kv.key, Value.of_otel_opt kv.value
