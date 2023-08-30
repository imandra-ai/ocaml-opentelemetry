module Otel := Opentelemetry
module Otrace := Trace
module TLS := Ambient_context_tls.Thread_local

val setup : unit -> unit
(** Install the OTEL backend as a Trace collector *)

val setup_with_otel_backend : Opentelemetry.Collector.backend -> unit
(** Same as {!setup},  but also install the given backend as OTEL backend *)

val collector : unit -> Trace.collector
(** Make a Trace collector that uses the OTEL backend to send spans and logs *)

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

    val enter_manual_span :
      parent:Otrace.explicit_span option ->
      flavor:'a ->
      __FUNCTION__:string option ->
      __FILE__:string ->
      __LINE__:int ->
      data:(string * Otrace.user_data) list ->
      string (* span name *) ->
      Otrace.explicit_span

    val exit_manual_span : Otrace.explicit_span -> unit

    val message :
      ?span:Otrace.span ->
      data:(string * Otrace.user_data) list ->
      string ->
      unit

    val shutdown : unit -> unit

    val name_process : string -> unit

    val name_thread : string -> unit

    val counter_int : string -> int -> unit

    val counter_float : string -> float -> unit
  end

  type span_begin = {
    id: Otel.Span_id.t;
    start_time: int64;
    name: string;
    data: (string * Otrace.user_data) list;
    __FILE__: string;
    __LINE__: int;
    __FUNCTION__: string option;
    trace_id: Otel.Trace_id.t;
    scope: Otel.Scope.t;
    parent_id: Otel.Span_id.t option;
    parent_scope: Otel.Scope.t option;
  }

  module Active_span_tbl : Hashtbl.S with type key = Otrace.span

  module Active_spans : sig
    type t = private { tbl: span_begin Active_span_tbl.t } [@@unboxed]

    val create : unit -> t

    val tls : t TLS.t

    val get : unit -> t
  end

  val otrace_of_otel : Otel.Span_id.t -> Otrace.span

  val otel_of_otrace : Otrace.span -> Otel.Span_id.t

  val spankind_of_string : string -> Otel.Span.kind

  val otel_attrs_of_otrace_data :
    (string * Otrace.user_data) list ->
    Otel.Span.kind * Otel.Span.key_value list

  val enter_span' :
    ?explicit_parent:Otrace.span ->
    __FUNCTION__:string option ->
    __FILE__:string ->
    __LINE__:int ->
    data:(string * Otrace.user_data) list ->
    string ->
    Otrace.span * span_begin

  val exit_span' : Otrace.span -> span_begin -> Otel.Span.t
end
