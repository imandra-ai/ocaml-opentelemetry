(** Basic random sampling *)

type t

val create : proba_accept:float -> unit -> t
(** [create ~proba_accept:n ()] makes a new sampler.

    The sampler will accept signals with probability [n] (must be between 0 and
    1).
    @raise Invalid_argument if [n] is not between 0 and 1. *)

val accept : t -> bool
(** Do we accept a sample? This returns [true] with probability [proba_accept].
*)

val proba_accept : t -> float

val actual_rate : t -> float
(** The ratio of signals we actually accepted so far *)

open Opentelemetry_emitter

val wrap_emitter : t -> 'a Emitter.t -> 'a Emitter.t
(** [wrap_emitter sampler e] is a new emitter that uses the [sampler] on each
    individual signal before passing them to [e]. *)
