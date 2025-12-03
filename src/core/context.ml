(** The context used in OTEL operations, to carry the current trace, etc.

    https://opentelemetry.io/docs/specs/otel/context/ *)

type t = Hmap.t
(** The context type. We use [Hmap.t] as it's standard and widely used. *)

type 'a key = 'a Hmap.key

let set = Hmap.add

(** @raise Invalid_argument if not present *)
let get_exn : 'a key -> t -> 'a = Hmap.get

let get : 'a key -> t -> 'a option = Hmap.find

let[@inline] new_key () : 'a key = Hmap.Key.create ()
