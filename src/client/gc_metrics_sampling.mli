val signal_we_need_gc_metrics : unit -> unit
(** External trigger to force the emission of GC metrics. Reentrant. *)

val pop_gc_metrics : unit -> Opentelemetry_proto.Metrics.resource_metrics list

val sample_gc_metrics_if_needed : unit -> unit
(** Make sure we sample GC metrics if needed (timeout, or signaled via
    {!signal_we_need_gc_metrics}) *)

