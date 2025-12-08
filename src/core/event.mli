(** Events.

    Events occur at a given time and can carry attributes. They always belong in
    a span. *)

open Common_
open Proto.Trace

type t = span_event

val make :
  ?time_unix_nano:Timestamp_ns.t -> ?attrs:Key_value.t list -> string -> t
