(** Storage implementation.

    There is a singleton storage for a given program, responsible for providing
    ambient context to the rest of the program. *)

type t = {
  name: string;
  get_context: unit -> Context.t;
  with_context: 'a. Context.t -> (unit -> 'a) -> 'a;
}
(** Storage type *)

(** Name of the storage implementation. *)
let[@inline] name self = self.name

(** Get the context from the current storage, or [Hmap.empty] if there is no
    ambient context. *)
let[@inline] get_context self = self.get_context ()

(** [with_context storage ctx f] calls [f()] in an ambient context in which
    [get_context()] will return [ctx]. Once [f()] returns, the storage is reset
    to its previous value. *)
let[@inline] with_context self ctx f = self.with_context ctx f

(** Get the ambient context and then look up [k] in it *)
let[@inline] get self (k : 'a Context.key) : 'a option =
  Hmap.find k (get_context self)

(** [with_key_bound_to storage k v f] calls [f()] in a context updated to have
    [k] map to [v]. *)
let with_key_bound_to self k v f =
  let ctx = get_context self in
  let new_ctx = Hmap.add k v ctx in
  self.with_context new_ctx f

(** [with_key_unbound storage k f] calls [f()] in a context updated to have [k]
    bound to no value. *)
let with_key_unbound self k f =
  let ctx = get_context self in
  if Hmap.mem k ctx then (
    let new_ctx = Hmap.rem k ctx in
    self.with_context new_ctx f
  ) else
    f ()
