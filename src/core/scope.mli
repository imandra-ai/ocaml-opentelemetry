(** Scopes.

    A scope is a trace ID and the span ID of the currently active span. *)

open Common_

type item_list

type t = {
  trace_id: Trace_id.t;
  span_id: Span_id.t;
  mutable items: item_list;
}

val attrs : t -> Key_value.t list

val events : t -> Event.t list

val links : t -> Span_link.t list

val status : t -> Span_status.t option

val kind : t -> Span_kind.t option

val make :
  trace_id:Trace_id.t ->
  span_id:Span_id.t ->
  ?events:Event.t list ->
  ?attrs:Key_value.t list ->
  ?links:Span_link.t list ->
  ?status:Span_status.t ->
  unit ->
  t

val to_span_link :
  ?trace_state:string ->
  ?attrs:Key_value.t list ->
  ?dropped_attributes_count:int ->
  t ->
  Span_link.t
(** Turn the scope into a span link *)

val to_span_ctx : t -> Span_ctx.t
(** Turn the scope into a span context *)

val add_event : t -> (unit -> Event.t) -> unit
(** Add an event to the scope. It will be aggregated into the span.

    Note that this takes a function that produces an event, and will only call
    it if there is an instrumentation backend. *)

val record_exception : t -> exn -> Printexc.raw_backtrace -> unit

val add_attrs : t -> (unit -> Key_value.t list) -> unit
(** Add attributes to the scope. It will be aggregated into the span.

    Note that this takes a function that produces attributes, and will only call
    it if there is an instrumentation backend. *)

val add_links : t -> (unit -> Span_link.t list) -> unit
(** Add links to the scope. It will be aggregated into the span.

    Note that this takes a function that produces links, and will only call it
    if there is an instrumentation backend. *)

val set_status : t -> Span_status.t -> unit
(** set the span status.

    Note that this function will be called only if there is an instrumentation
    backend. *)

val set_kind : t -> Span_kind.t -> unit
(** Set the span's kind.
    @since 0.11 *)

val ambient_scope_key : t Ambient_context.key
(** The opaque key necessary to access/set the ambient scope with
    {!Ambient_context}. *)

val get_ambient_scope : ?scope:t -> unit -> t option
(** Obtain current scope from {!Ambient_context}, if available. *)

val with_ambient_scope : t -> (unit -> 'a) -> 'a
(** [with_ambient_scope sc thunk] calls [thunk()] in a context where [sc] is the
    (thread|continuation)-local scope, then reverts to the previous local scope,
    if any.

    @see <https://github.com/ELLIOTTCABLE/ocaml-ambient-context>
      ambient-context docs *)
