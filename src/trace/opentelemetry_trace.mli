module Otel := Opentelemetry
module Otrace := Trace_core
module TLS := Thread_local_storage

module Conv : sig
  val trace_id_of_otel : Otel.Trace_id.t -> string

  val trace_id_to_otel : string -> Otel.Trace_id.t

  val span_id_of_otel : Otel.Span_id.t -> int64

  val span_id_to_otel : int64 -> Otel.Span_id.t

  val ctx_to_otel : Otrace.explicit_span_ctx -> Otel.Span_ctx.t

  val ctx_of_otel : Otel.Span_ctx.t -> Otrace.explicit_span_ctx
end

(** [opentelemetry.trace] implements a {!Trace_core.Collector} for
    {{:https://v3.ocaml.org/p/trace} ocaml-trace}.

    After installing this collector with {!setup}, you can consume libraries
    that use [ocaml-trace], and they will automatically emit OpenTelemetry spans
    and logs.

    Both explicit scope (in the [_manual] functions such as [enter_manual_span])
    and implicit scope (in {!Internal.M.with_span}, via {!Ambient_context}) are
    supported; see the detailed notes on {!Internal.M.enter_manual_span}.

    {1:wellknown Well-known identifiers}

    Because [ocaml-trace]'s API is a subset of OpenTelemetry functionality, this
    interface allows for a few 'well-known' identifiers to be used in
    [Trace]-instrumented libraries that wish to further support OpenTelemetry
    usage.

    (These strings will not change in subsequent versions of this library, so
    you do not need to depend on [opentelemetry.trace] to use them.)

    - If a key of exactly ["otrace.spankind"] is included in the
      {!Trace_core.user_data} passed to [with_span] et al., it will be used as
      the {!Opentelemetry.Span.kind} of the emitted span. (See
      {!Internal.spankind_of_string} for the list of supported values.)

    {[
      ocaml
      let describe () = [ Opentelemetry_trace.(spankind_key, client) ] in
      Trace_core.with_span ~__FILE__ ~__LINE__ ~data:describe "my-span"
      @@ fun _ ->
        (* ... *)
    ]} *)

val on_internal_error : (string -> unit) ref
(** Callback to print errors in the library itself (ie bugs) *)

val setup : unit -> unit
(** Install the OTEL backend as a Trace collector *)

val setup_with_otel_backend : Opentelemetry.Collector.backend -> unit
(** Same as {!setup}, but also install the given backend as OTEL backend *)

val collector : unit -> Trace_core.collector
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

(**/**)

(** Internal implementation details; do not consider these stable. *)
module Internal : sig
  module M : sig
    val with_span :
      __FUNCTION__:string option ->
      __FILE__:string ->
      __LINE__:int ->
      data:(string * Otrace.user_data) list ->
      string (* span name *) ->
      (Otrace.span -> 'a) ->
      'a
    (** Implements {!Trace_core.Collector.S.with_span}, with the OpenTelemetry
        collector as the backend. Invoked via {!Trace_core.with_span}.

        Notably, this has the same implicit-scope semantics as
        {!Opentelemetry.Trace.with_}, and requires configuration of
        {!Ambient_context}.

        @see <https://github.com/ELLIOTTCABLE/ocaml-ambient-context>
          ambient-context docs *)

    val enter_manual_span :
      parent:Otrace.explicit_span_ctx option ->
      flavor:'a ->
      __FUNCTION__:string option ->
      __FILE__:string ->
      __LINE__:int ->
      data:(string * Otrace.user_data) list ->
      string (* span name *) ->
      Otrace.explicit_span
    (** Implements {!Trace_core.Collector.S.enter_manual_span}, with the
        OpenTelemetry collector as the backend. Invoked at
        {!Trace_core.enter_manual_toplevel_span} and
        {!Trace_core.enter_manual_sub_span}; requires an eventual call to
        {!Trace_core.exit_manual_span}.

        These 'manual span' functions {e do not} implement the same implicit-
        scope semantics of {!with_span}; and thus don't need to wrap a single
        stack-frame / callback; you can freely enter a span at any point, store
        the returned {!Trace_core.explicit_span}, and exit it at any later point
        with {!Trace_core.exit_manual_span}.

        However, for that same reason, they also cannot update the
        {!Ambient_context} — that is, when you invoke the various [manual]
        functions, if you then invoke other functions that use
        {!Trace_core.with_span}, those callees {e will not} see the span you
        entered manually as their [parent].

        Generally, the best practice is to only use these [manual] functions at
        the 'leaves' of your callstack: that is, don't invoke user callbacks
        from within them; or if you do, make sure to pass the [explicit_span]
        you recieve from this function onwards to the user callback, so they can
        create further child-spans. *)

    val exit_manual_span : Otrace.explicit_span -> unit
    (** Implements {!Trace_core.Collector.S.exit_manual_span}, with the
        OpenTelemetry collector as the backend. Invoked at
        {!Trace_core.exit_manual_span}. Expects the [explicit_span] returned
        from an earlier call to {!Trace_core.enter_manual_toplevel_span} or
        {!Trace_core.enter_manual_sub_span}.

        (See the notes at {!enter_manual_span} about {!Ambient_context}.) *)

    val add_data_to_span :
      Otrace.span -> (string * Otrace.user_data) list -> unit

    val add_data_to_manual_span :
      Otrace.explicit_span -> (string * Otrace.user_data) list -> unit

    val message :
      ?span:Otrace.span ->
      data:(string * Otrace.user_data) list ->
      string ->
      unit

    val shutdown : unit -> unit

    val name_process : string -> unit

    val name_thread : string -> unit

    val counter_int :
      data:(string * Otrace.user_data) list -> string -> int -> unit

    val counter_float :
      data:(string * Otrace.user_data) list -> string -> float -> unit
  end

  type span_begin = {
    start_time: int64;
    name: string;
    __FILE__: string;
    __LINE__: int;
    __FUNCTION__: string option;
    scope: Otel.Scope.t;
    parent: Otel.Span_ctx.t option;
  }

  module Active_span_tbl : Hashtbl.S with type key = Otrace.span

  (** Table indexed by ocaml-trace spans. *)
  module Active_spans : sig
    type t = private { tbl: span_begin Active_span_tbl.t } [@@unboxed]

    val create : unit -> t

    val k_tls : t TLS.t

    val get : unit -> t
  end

  val otrace_of_otel : Otel.Span_id.t -> Otrace.span

  val enter_span' :
    ?explicit_parent:Otrace.explicit_span_ctx ->
    __FUNCTION__:string option ->
    __FILE__:string ->
    __LINE__:int ->
    data:(string * Otrace.user_data) list ->
    string ->
    Otrace.span * span_begin

  val exit_span' : Otrace.span -> span_begin -> Otel.Span.t
end

(**/**)
