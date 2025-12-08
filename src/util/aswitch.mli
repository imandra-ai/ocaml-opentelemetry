(** Aswitch for level-triggered cancellation and cleanup, atomically.

    A switch can be flipped to false once, and remains off forever afterwards.

    Inspired from https://ocsigen.org/lwt/5.5.0/api/Lwt_switch but thread-safe.
*)

type t

type trigger
(** Can be used to turn the switch off *)

val pp : Format.formatter -> t -> unit

val show : t -> string

val create : ?parent:t -> unit -> t * trigger
(** New switch.
    @param parent
      inherit from this switch. It means that the result switches off if the
      parent does, but conversely we can turn the result off without affecting
      the parent. In other words, this switch's lifetime is a subset of the
      parent's lifetime *)

val on_turn_off : t -> (unit -> unit) -> unit
(** [on_turn_off sw f] will call [f()] when [sw] is turned off. If [sw] is
    already off then [f()] is called immediately.

    {b NOTE} [f] really should not fail, and should be as fast and light as
    possible. *)

val is_on : t -> bool

val is_off : t -> bool

val turn_off : trigger -> unit

val turn_off' : trigger -> [ `Was_off | `Was_on ]
(** Turn off switch, return previous state *)

val link : t -> trigger -> unit
(** [link parent trigger] turns off [trigger] when [parent] is turned off *)

val dummy : t
(** Always off switch *)

module Unsafe : sig
  val trigger_of_switch : t -> trigger
  [@@alert unsafe "hope you know what you're doing"]
end
