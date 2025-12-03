(** [opentelemetry.trace] implements a {!Trace_core.Collector} for
    {{:https://v3.ocaml.org/p/trace} ocaml-trace}.

    After installing this collector with {!setup}, you can consume libraries
    that use [ocaml-trace], and they will automatically emit OpenTelemetry spans
    and logs.

    Both explicit scope (in the [_manual] functions such as [enter_manual_span])
    and implicit scope (in {!Internal.M.with_span}, via {!Ambient_context}) are
    supported; see the detailed notes on {!Internal.M.enter_manual_span}.

    We use [Trace_core.extension_event] to add more features on top of the
    common tracing interface. For example to set the "span kind":

    {[
      let@ span = Trace_core.with_span ~__FILE__ ~__LINE__ "my-span" in
      Opentelemetry_trace.set_span_kind span Span_kind_client
      (* ... *)
    ]} *)

module Otel := Opentelemetry
module Otrace := Trace_core
module TLS := Thread_local_storage

(** Conversions between [Opentelemetry] and [Trace_core] types *)
module Conv : sig
  val trace_id_of_otel : Otel.Trace_id.t -> string

  val trace_id_to_otel : string -> Otel.Trace_id.t

  val span_id_of_otel : Otel.Span_id.t -> int64

  val span_id_to_otel : int64 -> Otel.Span_id.t

  val ctx_to_otel : Otrace.explicit_span_ctx -> Otel.Span_ctx.t

  val ctx_of_otel : Otel.Span_ctx.t -> Otrace.explicit_span_ctx
end

(** The extension events for {!Trace_core}. *)
module Extensions : sig
  type Otrace.extension_event +=
    | Ev_link_span of Otrace.explicit_span * Otrace.explicit_span_ctx
          (** Link the given span to the given context. The context isn't the
              parent, but the link can be used to correlate both spans. *)
    | Ev_record_exn of {
        sp: Otrace.span;
        exn: exn;
        bt: Printexc.raw_backtrace;
        error: bool;  (** Is this an actual internal error? *)
      }
          (** Record exception and potentially turn span to an error *)
    | Ev_set_span_kind of Otrace.span * Otel.Span_kind.t
end

val on_internal_error : (string -> unit) ref
(** Callback to print errors in the library itself (ie bugs) *)

val setup : unit -> unit
(** Install the OTEL backend as a Trace collector *)

val setup_with_otel_exporter : #Opentelemetry.Exporter.t -> unit
(** Same as {!setup}, but using the given exporter *)

val setup_with_otel_backend : #Opentelemetry.Exporter.t -> unit
[@@deprecated "use setup_with_otel_exporter"]

val subscriber_of_exporter : #Otel.Exporter.t -> Trace_subscriber.t

val collector_of_exporter : #Otel.Exporter.t -> Trace_core.collector

val collector : unit -> Trace_core.collector
[@@deprecated "use collector_of_exporter, avoid global state"]
(** Make a Trace collector that uses the OTEL backend to send spans and logs *)

val link_spans : Otrace.explicit_span -> Otrace.explicit_span -> unit
(** [link_spans sp1 sp2] modifies [sp1] by adding a span link to [sp2].
    @since 0.11 *)

val set_span_kind : Otrace.explicit_span -> Otel.Span.kind -> unit
(** [set_span_kind sp k] sets the span's kind.
    @since 0.11 *)

val record_exception :
  Otrace.explicit_span -> exn -> Printexc.raw_backtrace -> unit
(** Record exception in the current span.
    @since 0.11 *)

(** Static references for well-known identifiers; see {!label-wellknown}. *)
module Well_known : sig
  val spankind_key : string

  val internal : Otrace.user_data

  val server : Otrace.user_data

  val client : Otrace.user_data

  val producer : Otrace.user_data

  val consumer : Otrace.user_data

  val spankind_of_string : string -> Otel.Span.kind

  val otel_attrs_of_otrace_data :
    (string * Otrace.user_data) list ->
    Otel.Span.kind * Otel.Span.key_value list
end
[@@deprecated "use the regular functions for this"]
