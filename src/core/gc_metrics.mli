(** Export GC metrics.

    These metrics are emitted regularly. *)

val get_metrics : unit -> Metrics.t list
(** Get a few metrics from the current state of the GC. *)

val setup : #Exporter.t -> unit
(** Setup a hook that will emit GC statistics on every tick. It does assume that
    [tick] is called regularly on the exporter. For example, if we ensure the
    exporter's [tick] function is called every 5s, we'll get GC metrics every
    5s. *)

val setup_on_main_exporter : unit -> unit
(** Setup the hook on the main exporter. *)

val basic_setup : unit -> unit [@@deprecated "use setup_on_main_exporter"]
