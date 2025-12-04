(** Export GC metrics.

    These metrics are emitted regularly. *)

val get_metrics : unit -> Metrics.t list
(** Get a few metrics from the current state of the GC. *)

val setup : ?min_interval_s:int -> Exporter.t -> unit
(** Setup a hook that will emit GC statistics on every tick. It does assume that
    [tick] is called regularly on the exporter. For example, if we ensure the
    exporter's [tick] function is called every 5s, we'll get GC metrics every
    5s.

    @param min_interval_s
      if provided, GC metrics will be emitted at most every [min_interval_s]
      seconds. This prevents flooding. Default value is 20s. *)

val setup_on_main_exporter : ?min_interval_s:int -> unit -> unit
(** Setup the hook on the main exporter. *)

val basic_setup : unit -> unit [@@deprecated "use setup_on_main_exporter"]
