(** Bounded queue based on {!Sync_queue} *)

val create : high_watermark:int -> unit -> 'a Bounded_queue.t
(** [create ~high_watermark ()] creates a new bounded queue based on
    {!Sync_queue} *)
