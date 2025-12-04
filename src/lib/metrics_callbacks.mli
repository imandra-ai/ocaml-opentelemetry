(** A set of callbacks that produce metrics when called. The metrics are
    automatically called regularly.

    This allows applications to register metrics callbacks from various points
    in the program (or even in libraries), and not worry about setting
    alarms/intervals to emit them. *)

type t

val create : unit -> t

val add_metrics_cb : t -> (unit -> Metrics.t list) -> unit
(** [register set f] adds the callback [f] to the [set].

    [f] will be called at unspecified times and is expected to return a list of
    metrics. It might be called regularly by the backend, in particular (but not
    only) when {!Exporter.tick} is called. *)

val add_to_exporter : Exporter.t -> t -> unit
(** Make sure we export metrics at every [tick] of the exporter *)

module Main_set : sig
  val get : unit -> t
  (** The global set *)
end
