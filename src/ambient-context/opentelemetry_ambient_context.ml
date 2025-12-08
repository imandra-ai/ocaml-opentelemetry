include Opentelemetry_ambient_context_core

let default_storage = Default_.storage

open struct
  (** The current ambient-context storage. *)
  let cur_storage : Storage.t Atomic.t = Atomic.make Default_.storage
end

let[@inline] get_current_storage () = Atomic.get cur_storage

(* NOTE: we can't really "map" each local context from the old
   to the new. Maybe the old storage is TLS based and the new one
   is per-lwt-task. *)
let set_current_storage (storage : Storage.t) = Atomic.set cur_storage storage

(** {2 Functions operating with the current storage} *)

(** Get the context from the current storage, or [Hmap.empty] if there is no
    ambient context. *)
let[@inline] get_context () = Storage.get_context (Atomic.get cur_storage)

(** [with_context ctx f] calls [f()] in an ambient context in which
    [get_context()] will return [ctx]. Once [f()] returns, the storage is reset
    to its previous value. *)
let[@inline] with_context ctx f =
  Storage.with_context (Atomic.get cur_storage) ctx f

(** Get the ambient context and then look up [k] in it *)
let[@inline] get (k : 'a Context.key) : 'a option = Hmap.find k (get_context ())

(** [with_key_bound_to storage k v f] calls [f()] in a context updated to have
    [k] map to [v]. *)
let with_key_bound_to k v f =
  let storage = get_current_storage () in
  let ctx = Storage.get_context storage in
  let new_ctx = Hmap.add k v ctx in
  Storage.with_context storage new_ctx f

(** [with_key_unbound k f] calls [f()] in a context updated to have [k] bound to
    no value. *)
let with_key_unbound k f =
  let storage = Atomic.get cur_storage in
  let ctx = Storage.get_context storage in
  if Hmap.mem k ctx then (
    let new_ctx = Hmap.rem k ctx in
    Storage.with_context storage new_ctx f
  ) else
    f ()
