(** Thread/Domain local storage

    This allows the creation of global state that is per-domain or per-thread.
*)

type 'a t

val create : unit -> 'a t
(** Create new storage *)

val get : 'a t -> 'a option
(** Get current value *)

val get_exn : 'a t -> 'a
(** Like {!get} but fails with an exception
    @raise Not_found if no value was found *)

val set : 'a t -> 'a -> unit

val remove : _ t -> unit

val get_or_create : create:(unit -> 'a) -> 'a t -> 'a

val with_ : 'a t -> 'a -> ('a option -> 'b) -> 'b
(** [with_ var x f] sets [var] to [x] for this thread, calls [f prev] where
    [prev] is the value currently in [var] (if any), and
    then restores the old value of [var] for this thread. *)
