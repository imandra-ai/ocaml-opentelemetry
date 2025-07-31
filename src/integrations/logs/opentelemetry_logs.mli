val emit_telemetry_tag : bool Logs.Tag.def
(** [emit_telemetry_tag] is a logging tag that when applied to a log, determines
    if a log will be emitted by the tracing/telemetry backend.

    Since some OTel backends can cause deadlocks if used during a GC alarm, this
    is useful for when you may want logging during a GC alarm. It is also useful
    if you want to log sensitive information, but not report it via
    Opentelemetry.

    If this tag is not set on a log, said log will be emitted by default if the
    otel reporter is registered.

    Example:
    {[
      let tags =
        Logs.Tag.(add Opentelemetry_logs.emit_telemetry_tag false other_tags)
      in
      Logs.info (fun m ->
          m ~tags "This log will not be sent to the telemetry backend")
    ]} *)

val emit_telemetry : bool -> Logs.Tag.set
(** [emit_telemetry emit_or_not] creates a tag set with the
    {!emit_telemetry_tag} as its only member *)

val otel_reporter :
  ?service_name:string ->
  ?attributes:(string * Opentelemetry.value) list ->
  unit ->
  Logs.reporter
(** [otel_reporter ?service_name ?tag_value_pp_buffer_size ?attrs ()] creates a
    [Logs.reporter] that will create and emit an OTel log with the following
    info:
    {ul
     {- Log severity is converted to OTel severity as follows:
        {[
          module Otel = Opentelemetry
          match level with
          | Logs.App -> Otel.Logs.Severity_number_info (* like info, but less severe  *)
          | Logs.Info -> Otel.Logs.Severity_number_info2
          | Logs.Error -> Otel.Logs.Severity_number_error
          | Logs.Warning -> Otel.Logs.Severity_number_warn
          | Logs.Debug -> Otel.Logs.Severity_number_debug
        ]}
     }
     {- message is the formatted with the given [fmt] and [msgf] function, and
        emitted as the log body
     }
     {- [header] and [src] will be added as attributes
        [("header", `String header)] and [("src", `String (Logs.Src.name src))]
        respectively
     }
     {- [tags] will be also added as attributes, with the tag name as the key,
        and the value formatted via its formatter as the value.
     }
     {- [attributes] will also be added as attributes, and are useful for
        setting static attributes such as a library name
     }
    }

    Example use: [Logs.set_reporter (Opentelemetery_logs.otel_reporter ())] *)

val attach_otel_reporter :
  ?service_name:string ->
  ?attributes:(string * Opentelemetry.value) list ->
  Logs.reporter ->
  Logs.reporter
(** [attach_otel_reporter ?service_name ?attributes reporter] will create a
    reporter that first calls the reporter passed as an argument, then an otel
    report created via {!otel_reporter}, for every log. This is useful for if
    you want to emit logs to stderr and to OTel at the same time.

    Example:
    {[
      let reporter = Logs_fmt.reporter () in
      Logs.set_reporter
        (Opentelemetry_logs.attach_otel_reporter ?service_name ?attributes
           reporter)
    ]} *)
