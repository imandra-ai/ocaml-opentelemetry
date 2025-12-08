(** Bounded queue based on simple synchronization primitives.

    This is not the fastest queue but it should be versatile. *)

val create : high_watermark:int -> unit -> 'a Bounded_queue.t
(** [create ~high_watermark ()] creates a new bounded queue based on
    {!Sync_queue} *)
