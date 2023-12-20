(** Opentelemetry types and instrumentation *)

module Lock = Lock
(** Global lock. *)

module Rand_bytes = Rand_bytes
(** Generation of random identifiers. *)

module AList = AList
(** Atomic list, for internal usage
    @since NEXT_RELEASE *)

open struct
  let[@inline] result_bind x f =
    match x with
    | Error e -> Error e
    | Ok x -> f x
end

(** {2 Wire format} *)

module Proto = Opentelemetry_proto
(** Protobuf types.

   This is mostly useful internally. Users should not need to touch it. *)

(** {2 Timestamps} *)

(** Unix timestamp.

    These timestamps measure time since the Unix epoch (jan 1, 1970) UTC
    in nanoseconds. *)
module Timestamp_ns = struct
  type t = int64

  let ns_in_a_day = Int64.(mul 1_000_000_000L (of_int (24 * 3600)))

  (** Current unix timestamp in nanoseconds *)
  let[@inline] now_unix_ns () : t =
    let span = Ptime_clock.now () |> Ptime.to_span in
    let d, ps = Ptime.Span.to_d_ps span in
    let d = Int64.(mul (of_int d) ns_in_a_day) in
    let ns = Int64.(div ps 1_000L) in
    Int64.(add d ns)
end

(** {2 Interface to data collector} *)

(** Collector types

    These types are used by backend implementations, to send events to
    collectors such as Jaeger.

    Note: most users will not need to touch this module *)
module Collector = struct
  open Opentelemetry_proto

  type 'msg sender = { send: 'a. 'msg -> ret:(unit -> 'a) -> 'a }
  (** Sender interface for a message of type [msg].
      Inspired from Logs' reporter
      (see {{:https://erratique.ch/software/logs/doc/Logs/index.html#sync} its doc})
      but without [over] as it doesn't make much sense in presence
      of batching.

      The [ret] callback is used to return the desired type (unit, or
      a Lwt promise, or anything else) once the event has been transferred
      to the backend.
      It doesn't mean the event has been collected yet, it
      could sit in a batch queue for a little while.
  *)

  (** Collector client interface. *)
  module type BACKEND = sig
    val send_trace : Trace.resource_spans list sender

    val send_metrics : Metrics.resource_metrics list sender

    val send_logs : Logs.resource_logs list sender

    val signal_emit_gc_metrics : unit -> unit
    (** Signal the backend that it should emit GC metrics when it has the
        chance. This should be installed in a GC alarm or another form
        of regular trigger. *)

    val tick : unit -> unit
    (** Should be called regularly for background processing,
        timeout checks, etc. *)

    val set_on_tick_callbacks : (unit -> unit) list ref -> unit
    (** Give the collector the list of callbacks to be executed
        when [tick()] is called. Each such callback should be short and
        reentrant. Depending on the collector's implementation, it might be
        called from a thread that is not the one that called [on_tick]. *)

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  module Noop_backend : BACKEND = struct
    let noop_sender _ ~ret = ret ()

    let send_trace : Trace.resource_spans list sender = { send = noop_sender }

    let send_metrics : Metrics.resource_metrics list sender =
      { send = noop_sender }

    let send_logs : Logs.resource_logs list sender = { send = noop_sender }

    let signal_emit_gc_metrics () = ()

    let tick () = ()

    let set_on_tick_callbacks _cbs = ()

    let cleanup () = ()
  end

  module Debug_backend (B : BACKEND) : BACKEND = struct
    open Proto

    let send_trace : Trace.resource_spans list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "SPANS: %a@."
              (Format.pp_print_list Trace.pp_resource_spans)
              l;
            B.send_trace.send l ~ret);
      }

    let send_metrics : Metrics.resource_metrics list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "METRICS: %a@."
              (Format.pp_print_list Metrics.pp_resource_metrics)
              l;
            B.send_metrics.send l ~ret);
      }

    let send_logs : Logs.resource_logs list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "LOGS: %a@."
              (Format.pp_print_list Logs.pp_resource_logs)
              l;
            B.send_logs.send l ~ret);
      }

    let signal_emit_gc_metrics () = B.signal_emit_gc_metrics ()

    let tick () = B.tick ()

    let set_on_tick_callbacks cbs = B.set_on_tick_callbacks cbs

    let cleanup () = B.cleanup ()
  end

  let debug_backend : backend = (module Debug_backend (Noop_backend))

  (* hidden *)
  open struct
    let on_tick_cbs_ = ref []

    let backend : backend option ref = ref None
  end

  (** Set collector backend *)
  let set_backend (b : backend) : unit =
    let (module B) = b in
    B.set_on_tick_callbacks on_tick_cbs_;
    backend := Some b

  (** Is there a configured backend? *)
  let[@inline] has_backend () : bool = !backend != None

  (** Current backend, if any *)
  let[@inline] get_backend () : backend option = !backend

  let send_trace (l : Trace.resource_spans list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_trace.send l ~ret

  let send_metrics (l : Metrics.resource_metrics list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_metrics.send l ~ret

  let send_logs (l : Logs.resource_logs list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_logs.send l ~ret

  let[@inline] rand_bytes_16 () = !Rand_bytes.rand_bytes_16 ()

  let[@inline] rand_bytes_8 () = !Rand_bytes.rand_bytes_8 ()

  let on_tick f = on_tick_cbs_ := f :: !on_tick_cbs_

  (** Do background work. Call this regularly if the collector doesn't
      already have a ticker thread or internal timer. *)
  let tick () =
    match !backend with
    | None -> ()
    | Some (module B) -> B.tick ()

  let with_setup_debug_backend b ?(enable = true) () f =
    let (module B : BACKEND) = b in
    if enable then (
      set_backend b;
      Fun.protect ~finally:B.cleanup f
    ) else
      f ()
end

module Util_ = struct
  let bytes_to_hex (b : bytes) : string =
    let i_to_hex (i : int) =
      if i < 10 then
        Char.chr (i + Char.code '0')
      else
        Char.chr (i - 10 + Char.code 'a')
    in

    let res = Bytes.create (2 * Bytes.length b) in
    for i = 0 to Bytes.length b - 1 do
      let n = Char.code (Bytes.get b i) in
      Bytes.set res (2 * i) (i_to_hex ((n land 0xf0) lsr 4));
      Bytes.set res ((2 * i) + 1) (i_to_hex (n land 0x0f))
    done;
    Bytes.unsafe_to_string res

  let bytes_of_hex (s : string) : bytes =
    let n_of_c = function
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
      | _ -> raise (Invalid_argument "invalid hex char")
    in
    if String.length s mod 2 <> 0 then
      raise (Invalid_argument "hex sequence must be of even length");
    let res = Bytes.make (String.length s / 2) '\x00' in
    for i = 0 to (String.length s / 2) - 1 do
      let n1 = n_of_c (String.get s (2 * i)) in
      let n2 = n_of_c (String.get s ((2 * i) + 1)) in
      let n = (n1 lsl 4) lor n2 in
      Bytes.set res i (Char.chr n)
    done;
    res
end

(** {2 Identifiers} *)

(** Trace ID.

    This 16 bytes identifier is shared by all spans in one trace. *)
module Trace_id : sig
  type t

  val create : unit -> t

  val pp : Format.formatter -> t -> unit

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t

  val to_hex : t -> string

  val of_hex : string -> t
end = struct
  type t = bytes

  let to_bytes self = self

  let create () : t =
    let b = Collector.rand_bytes_16 () in
    assert (Bytes.length b = 16);
    (* make sure the identifier is not all 0, which is a dummy identifier. *)
    Bytes.set b 0 (Char.unsafe_chr (Char.code (Bytes.get b 0) lor 1));
    b

  let of_bytes b =
    if Bytes.length b = 16 then
      b
    else
      raise (Invalid_argument "trace IDs must be 16 bytes in length")

  let to_hex self = Util_.bytes_to_hex self

  let of_hex s = of_bytes (Util_.bytes_of_hex s)

  let pp fmt t = Format.fprintf fmt "%s" (to_hex t)
end

(** Unique ID of a span. *)
module Span_id : sig
  type t

  val create : unit -> t

  val pp : Format.formatter -> t -> unit

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t

  val to_hex : t -> string

  val of_hex : string -> t
end = struct
  type t = bytes

  let to_bytes self = self

  let create () : t =
    let b = Collector.rand_bytes_8 () in
    assert (Bytes.length b = 8);
    (* make sure the identifier is not all 0, which is a dummy identifier. *)
    Bytes.set b 0 (Char.unsafe_chr (Char.code (Bytes.get b 0) lor 1));
    b

  let of_bytes b =
    if Bytes.length b = 8 then
      b
    else
      raise (Invalid_argument "span IDs must be 8 bytes in length")

  let to_hex self = Util_.bytes_to_hex self

  let of_hex s = of_bytes (Util_.bytes_of_hex s)

  let pp fmt t = Format.fprintf fmt "%s" (to_hex t)
end

(** {2 Attributes and conventions} *)

module Conventions = struct
  module Attributes = struct
    module Process = struct
      module Runtime = struct
        let name = "process.runtime.name"

        let version = "process.runtime.version"

        let description = "process.runtime.description"
      end
    end

    module Service = struct
      let name = "service.name"

      let namespace = "service.namespace"

      let instance_id = "service.instance.id"

      let version = "service.version"
    end
  end

  module Metrics = struct
    module Process = struct
      module Runtime = struct
        module Ocaml = struct
          module GC = struct
            let compactions = "process.runtime.ocaml.gc.compactions"

            let major_collections = "process.runtime.ocaml.gc.major_collections"

            let major_heap = "process.runtime.ocaml.gc.major_heap"

            let minor_allocated = "process.runtime.ocaml.gc.minor_allocated"

            let minor_collections = "process.runtime.ocaml.gc.minor_collections"
          end
        end
      end
    end
  end
end

type value =
  [ `Int of int
  | `String of string
  | `Bool of bool
  | `Float of float
  | `None
  ]

type key_value = string * value

(**/**)

let _conv_value =
  let open Proto.Common in
  function
  | `Int i -> Some (Int_value (Int64.of_int i))
  | `String s -> Some (String_value s)
  | `Bool b -> Some (Bool_value b)
  | `Float f -> Some (Double_value f)
  | `None -> None

(**/**)

(**/**)

let _conv_key_value (k, v) =
  let open Proto.Common in
  let value = _conv_value v in
  default_key_value ~key:k ~value ()

(**/**)

(** {2 Global settings} *)

(** Process-wide metadata, environment variables, etc. *)
module Globals = struct
  open Proto.Common

  (** Main service name metadata *)
  let service_name = ref "unknown_service"

  (** Namespace for the service *)
  let service_namespace = ref None

  (** Unique identifier for the service *)
  let service_instance_id = ref None

  let instrumentation_library =
    default_instrumentation_scope ~version:"0.2" ~name:"ocaml-otel" ()

  (** Global attributes, initially set
      via OTEL_RESOURCE_ATTRIBUTES and modifiable
      by the user code. They will be attached to each outgoing metrics/traces. *)
  let global_attributes : key_value list ref =
    let parse_pair s =
      match String.split_on_char '=' s with
      | [ a; b ] -> default_key_value ~key:a ~value:(Some (String_value b)) ()
      | _ -> failwith (Printf.sprintf "invalid attribute: %S" s)
    in
    ref
    @@
    try
      Sys.getenv "OTEL_RESOURCE_ATTRIBUTES"
      |> String.split_on_char ',' |> List.map parse_pair
    with _ -> []

  (** Add a global attribute *)
  let add_global_attribute (key : string) (v : value) : unit =
    global_attributes := _conv_key_value (key, v) :: !global_attributes

  (* add global attributes to this list *)
  let merge_global_attributes_ into : _ list =
    let not_redundant kv = List.for_all (fun kv' -> kv.key <> kv'.key) into in
    List.rev_append (List.filter not_redundant !global_attributes) into

  (** Default span kind in {!Span.create}.
      This will be used in all spans that do not specify [~kind] explicitly;
      it is set to "internal", following directions from the [.proto] file.
      It can be convenient to set "client" or "server" uniformly in here.
      @since 0.4 *)
  let default_span_kind = ref Proto.Trace.Span_kind_internal

  let mk_attributes ?(service_name = !service_name) ?(attrs = []) () : _ list =
    let l = List.map _conv_key_value attrs in
    let l =
      default_key_value ~key:Conventions.Attributes.Service.name
        ~value:(Some (String_value service_name)) ()
      :: l
    in
    let l =
      match !service_instance_id with
      | None -> l
      | Some v ->
        default_key_value ~key:Conventions.Attributes.Service.instance_id
          ~value:(Some (String_value v)) ()
        :: l
    in
    let l =
      match !service_namespace with
      | None -> l
      | Some v ->
        default_key_value ~key:Conventions.Attributes.Service.namespace
          ~value:(Some (String_value v)) ()
        :: l
    in
    l |> merge_global_attributes_
end

(** {2 Traces and Spans} *)

(** Events.

    Events occur at a given time and can carry attributes. They always
    belong in a span. *)
module Event : sig
  open Proto.Trace

  type t = span_event

  val make :
    ?time_unix_nano:Timestamp_ns.t -> ?attrs:key_value list -> string -> t
end = struct
  open Proto.Trace

  type t = span_event

  let make ?(time_unix_nano = Timestamp_ns.now_unix_ns ()) ?(attrs = [])
      (name : string) : t =
    let attrs = List.map _conv_key_value attrs in
    default_span_event ~time_unix_nano ~name ~attributes:attrs ()
end

(** {2 Scopes} *)

(** Scopes.

    A scope is a trace ID and the span ID of the currently active span.
*)
module Scope = struct
  type t = {
    trace_id: Trace_id.t;
    span_id: Span_id.t;
    mutable events: Event.t list;
    mutable attrs: key_value list;
  }

  (** Add an event to the scope. It will be aggregated into the span.

      Note that this takes a function that produces an event, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_event (scope : t) (ev : unit -> Event.t) : unit =
    if Collector.has_backend () then scope.events <- ev () :: scope.events

  (** Add an attr to the scope. It will be aggregated into the span.

      Note that this takes a function that produces attributes, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_attrs (scope : t) (attrs : unit -> key_value list) : unit =
    if Collector.has_backend () then
      scope.attrs <- List.rev_append (attrs ()) scope.attrs

  (** The opaque key necessary to access/set the ambient scope with
      {!Ambient_context}. *)
  let ambient_scope_key : t Ambient_context.key = Ambient_context.create_key ()

  (** Obtain current scope from {!Ambient_context}, if available. *)
  let get_ambient_scope ?scope () : t option =
    match scope with
    | Some _ -> scope
    | None -> Ambient_context.get ambient_scope_key

  (** [with_ambient_scope sc thunk] calls [thunk()] in a context where [sc] is
      the (thread|continuation)-local scope, then reverts to the previous local
      scope, if any.

      @see <https://github.com/ELLIOTTCABLE/ocaml-ambient-context> ambient-context docs *)
  let[@inline] with_ambient_scope (sc : t) (f : unit -> 'a) : 'a =
    Ambient_context.with_binding ambient_scope_key sc (fun _ -> f ())
end

(** Span Link

   A pointer from the current span to another span in the same trace or in a
   different trace. For example, this can be used in batching operations,
   where a single batch handler processes multiple requests from different
   traces or when the handler receives a request from a different project.
*)
module Span_link : sig
  open Proto.Trace

  type t = span_link

  val make :
    trace_id:Trace_id.t ->
    span_id:Span_id.t ->
    ?trace_state:string ->
    ?attrs:key_value list ->
    ?dropped_attributes_count:int ->
    unit ->
    t
end = struct
  open Proto.Trace

  type t = span_link

  let make ~trace_id ~span_id ?trace_state ?(attrs = [])
      ?dropped_attributes_count () : t =
    let attributes = List.map _conv_key_value attrs in
    let dropped_attributes_count =
      Option.map Int32.of_int dropped_attributes_count
    in
    default_span_link
      ~trace_id:(Trace_id.to_bytes trace_id)
      ~span_id:(Span_id.to_bytes span_id) ?trace_state ~attributes
      ?dropped_attributes_count ()
end

(** Spans.

    A Span is the workhorse of traces, it indicates an operation that
    took place over a given span of time (indicated by start_time and end_time)
    as part of a hierarchical trace. All spans in a given trace are bound by
    the use of the same {!Trace_id.t}. *)
module Span : sig
  open Proto.Trace

  type t = span

  type id = Span_id.t

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type nonrec status_code = status_status_code =
    | Status_code_unset
    | Status_code_ok
    | Status_code_error

  type nonrec status = status = {
    message: string;
    code: status_code;
  }

  val id : t -> Span_id.t

  type key_value =
    string
    * [ `Int of int
      | `String of string
      | `Bool of bool
      | `Float of float
      | `None
      ]

  val create :
    ?kind:kind ->
    ?id:id ->
    ?trace_state:string ->
    ?attrs:key_value list ->
    ?events:Event.t list ->
    ?status:status ->
    trace_id:Trace_id.t ->
    ?parent:id ->
    ?links:Span_link.t list ->
    start_time:Timestamp_ns.t ->
    end_time:Timestamp_ns.t ->
    string ->
    t * id
  (** [create ~trace_id name] creates a new span with its unique ID.
        @param trace_id the trace this belongs to
        @param parent parent span, if any
        @param links list of links to other spans, each with their trace state
        (see {{: https://www.w3.org/TR/trace-context/#tracestate-header} w3.org}) *)
end = struct
  open Proto.Trace

  type t = span

  type id = Span_id.t

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type key_value =
    string
    * [ `Int of int
      | `String of string
      | `Bool of bool
      | `Float of float
      | `None
      ]

  type nonrec status_code = status_status_code =
    | Status_code_unset
    | Status_code_ok
    | Status_code_error

  type nonrec status = status = {
    message: string;
    code: status_code;
  }

  let id self = Span_id.of_bytes self.span_id

  let create ?(kind = !Globals.default_span_kind) ?(id = Span_id.create ())
      ?trace_state ?(attrs = []) ?(events = []) ?status ~trace_id ?parent
      ?(links = []) ~start_time ~end_time name : t * id =
    let trace_id = Trace_id.to_bytes trace_id in
    let parent_span_id = Option.map Span_id.to_bytes parent in
    let attributes = List.map _conv_key_value attrs in
    let span =
      default_span ~trace_id ?parent_span_id ~span_id:(Span_id.to_bytes id)
        ~attributes ~events ?trace_state ~status ~kind ~name ~links
        ~start_time_unix_nano:start_time ~end_time_unix_nano:end_time ()
    in
    span, id
end

(** Traces.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal} the spec} *)
module Trace = struct
  open Proto.Trace

  type span = Span.t

  let make_resource_spans ?service_name ?attrs spans =
    let ils =
      default_scope_spans ~scope:(Some Globals.instrumentation_library) ~spans
        ()
    in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_spans ~resource:(Some resource) ~scope_spans:[ ils ] ()

  (** Sync emitter.

      This instructs the collector to forward
      the spans to some backend at a later point.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks. *)
  let emit ?service_name ?attrs (spans : span list) : unit =
    let rs = make_resource_spans ?service_name ?attrs spans in
    Collector.send_trace [ rs ] ~ret:(fun () -> ())

  type scope = Scope.t = {
    trace_id: Trace_id.t;
    span_id: Span_id.t;
    mutable events: Event.t list;
    mutable attrs: Span.key_value list;
  }
  [@@deprecated "use Scope.t"]

  let add_event = Scope.add_event [@@deprecated "use Scope.add_event"]

  let add_attrs = Scope.add_attrs [@@deprecated "use Scope.add_attrs"]

  let with_' ?(force_new_trace_id = false) ?trace_state ?service_name
      ?(attrs : (string * [< value ]) list = []) ?kind ?trace_id ?parent ?scope
      ?links name cb =
    let scope =
      if force_new_trace_id then
        None
      else
        Scope.get_ambient_scope ?scope ()
    in
    let trace_id =
      match trace_id, scope with
      | _ when force_new_trace_id -> Trace_id.create ()
      | Some trace_id, _ -> trace_id
      | None, Some scope -> scope.trace_id
      | None, None -> Trace_id.create ()
    in
    let parent =
      match parent, scope with
      | _ when force_new_trace_id -> None
      | Some span_id, _ -> Some span_id
      | None, Some scope -> Some scope.span_id
      | None, None -> None
    in
    let start_time = Timestamp_ns.now_unix_ns () in
    let span_id = Span_id.create () in
    let scope = { trace_id; span_id; events = []; attrs } in
    (* called once we're done, to emit a span *)
    let finally res =
      let status =
        match res with
        | Ok () -> default_status ~code:Status_code_ok ()
        | Error e -> default_status ~code:Status_code_error ~message:e ()
      in
      let span, _ =
        (* TODO: should the attrs passed to with_ go on the Span
           (in Span.create) or on the ResourceSpan (in emit)?
           (question also applies to Opentelemetry_lwt.Trace.with) *)
        Span.create ?kind ~trace_id ?parent ?links ~id:span_id ?trace_state
          ~attrs:scope.attrs ~events:scope.events ~start_time
          ~end_time:(Timestamp_ns.now_unix_ns ())
          ~status name
      in
      emit ?service_name [ span ]
    in
    let thunk () =
      (* set global scope in this thread *)
      Scope.with_ambient_scope scope @@ fun () -> cb scope
    in
    thunk, finally

  (** Sync span guard.

      Notably, this includes {e implicit} scope-tracking: if called without a
      [~scope] argument (or [~parent]/[~trace_id]), it will check in the
      {!Ambient_context} for a surrounding environment, and use that as the
      scope. Similarly, it uses {!Scope.with_ambient_scope} to {e set} a new
      scope in the ambient context, so that any logically-nested calls to
      {!with_} will use this span as their parent.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks.

      @param force_new_trace_id if true (default false), the span will not use a
      ambient scope, the [~scope] argument, nor [~trace_id], but will instead
      always create fresh identifiers for this span *)

  let with_ ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind
      ?trace_id ?parent ?scope ?links name (cb : Scope.t -> 'a) : 'a =
    let thunk, finally =
      with_' ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind
        ?trace_id ?parent ?scope ?links name cb
    in

    try
      let rv = thunk () in
      finally (Ok ());
      rv
    with e ->
      finally (Error (Printexc.to_string e));
      raise e
end

(** {2 Metrics} *)

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  open Proto
  open Proto.Metrics

  type t = Metrics.metric
  (** A single metric, measuring some time-varying quantity or statistical
      distribution. It is composed of one or more data points that have
      precise values and time stamps. Each distinct metric should have a
      distinct name. *)

  open struct
    let _program_start = Timestamp_ns.now_unix_ns ()
  end

  (** Number data point, as a float *)
  let float ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) (d : float) :
      number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes ~value:(As_double d) ()

  (** Number data point, as an int *)
  let int ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) (i : int) :
      number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes
      ~value:(As_int (Int64.of_int i))
      ()

  (** Aggregation of a scalar metric, always with the current value *)
  let gauge ~name ?description ?unit_ (l : number_data_point list) : t =
    let data = Gauge (default_gauge ~data_points:l ()) in
    default_metric ~name ?description ?unit_ ~data ()

  type aggregation_temporality = Metrics.aggregation_temporality =
    | Aggregation_temporality_unspecified
    | Aggregation_temporality_delta
    | Aggregation_temporality_cumulative

  (** Sum of all reported measurements over a time interval *)
  let sum ~name ?description ?unit_
      ?(aggregation_temporality = Aggregation_temporality_cumulative)
      ?is_monotonic (l : number_data_point list) : t =
    let data =
      Sum (default_sum ~data_points:l ?is_monotonic ~aggregation_temporality ())
    in
    default_metric ~name ?description ?unit_ ~data ()

  (** Histogram data
      @param count number of values in population (non negative)
      @param sum sum of values in population (0 if count is 0)
      @param bucket_counts count value of histogram for each bucket. Sum of
      the counts must be equal to [count].
      length must be [1+length explicit_bounds]
      @param explicit_bounds strictly increasing list of bounds for the buckets *)
  let histogram_data_point ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) ?(exemplars = [])
      ?(explicit_bounds = []) ?sum ~bucket_counts ~count () :
      histogram_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_histogram_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes ~exemplars ~bucket_counts ~explicit_bounds ~count ?sum ()

  let histogram ~name ?description ?unit_ ?aggregation_temporality
      (l : histogram_data_point list) : t =
    let data =
      Histogram (default_histogram ~data_points:l ?aggregation_temporality ())
    in
    default_metric ~name ?description ?unit_ ~data ()

  (* TODO: exponential history *)
  (* TODO: summary *)
  (* TODO: exemplar *)

  (** Aggregate metrics into a {!Proto.Metrics.resource_metrics} *)
  let make_resource_metrics ?service_name ?attrs (l : t list) : resource_metrics
      =
    let lm =
      default_scope_metrics ~scope:(Some Globals.instrumentation_library)
        ~metrics:l ()
    in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_metrics ~scope_metrics:[ lm ] ~resource:(Some resource) ()

  (** Emit some metrics to the collector (sync). This blocks until
      the backend has pushed the metrics into some internal queue, or
      discarded them.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks.
      *)
  let emit ?attrs (l : t list) : unit =
    let rm = make_resource_metrics ?attrs l in
    Collector.send_metrics [ rm ] ~ret:ignore
end

(** Logs.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#log-signal} the spec} *)
module Logs = struct
  open Opentelemetry_proto
  open Logs

  type t = log_record

  (** Severity level of a log event *)
  type severity = Logs.severity_number =
    | Severity_number_unspecified
    | Severity_number_trace
    | Severity_number_trace2
    | Severity_number_trace3
    | Severity_number_trace4
    | Severity_number_debug
    | Severity_number_debug2
    | Severity_number_debug3
    | Severity_number_debug4
    | Severity_number_info
    | Severity_number_info2
    | Severity_number_info3
    | Severity_number_info4
    | Severity_number_warn
    | Severity_number_warn2
    | Severity_number_warn3
    | Severity_number_warn4
    | Severity_number_error
    | Severity_number_error2
    | Severity_number_error3
    | Severity_number_error4
    | Severity_number_fatal
    | Severity_number_fatal2
    | Severity_number_fatal3
    | Severity_number_fatal4

  let pp_severity = Logs.pp_severity_number

  type flags = Logs.log_record_flags =
    | Log_record_flags_do_not_use
    | Log_record_flags_trace_flags_mask

  let pp_flags = Logs.pp_log_record_flags

  (** Make a single log entry *)
  let make ?time ?(observed_time_unix_nano = Timestamp_ns.now_unix_ns ())
      ?severity ?log_level ?flags ?trace_id ?span_id (body : value) : t =
    let time_unix_nano =
      match time with
      | None -> observed_time_unix_nano
      | Some t -> t
    in
    let trace_id = Option.map Trace_id.to_bytes trace_id in
    let span_id = Option.map Span_id.to_bytes span_id in
    let body = _conv_value body in
    default_log_record ~time_unix_nano ~observed_time_unix_nano
      ?severity_number:severity ?severity_text:log_level ?flags ?trace_id
      ?span_id ~body ()

  (** Make a log entry whose body is a string *)
  let make_str ?time ?observed_time_unix_nano ?severity ?log_level ?flags
      ?trace_id ?span_id (body : string) : t =
    make ?time ?observed_time_unix_nano ?severity ?log_level ?flags ?trace_id
      ?span_id (`String body)

  (** Make a log entry with format *)
  let make_strf ?time ?observed_time_unix_nano ?severity ?log_level ?flags
      ?trace_id ?span_id fmt =
    Format.kasprintf
      (fun bod ->
        make_str ?time ?observed_time_unix_nano ?severity ?log_level ?flags
          ?trace_id ?span_id bod)
      fmt

  (** Emit logs.

    This instructs the collector to send the logs to some backend at
    a later date.
    {b NOTE} be careful not to call this inside a Gc alarm, as it can
    cause deadlocks. *)
  let emit ?service_name ?attrs (l : t list) : unit =
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    let ll =
      default_scope_logs ~scope:(Some Globals.instrumentation_library)
        ~log_records:l ()
    in
    let rl =
      default_resource_logs ~resource:(Some resource) ~scope_logs:[ ll ] ()
    in
    Collector.send_logs [ rl ] ~ret:ignore
end

(** A set of callbacks that produce metrics when called.
    The metrics are automatically called regularly.

    This allows applications to register metrics callbacks from various points
    in the program (or even in libraries), and not worry about setting
    alarms/intervals to emit them. *)
module Metrics_callbacks = struct
  open struct
    let cbs_ : (unit -> Metrics.t list) list ref = ref []
  end

  (** [register f] adds the callback [f] to the list.

      [f] will be called at unspecified times and is expected to return
      a list of metrics. It might be called regularly by the backend,
      in particular (but not only) when {!Collector.tick} is called. *)
  let register f : unit =
    if !cbs_ = [] then
      (* make sure we call [f] (and others) at each tick *)
      Collector.on_tick (fun () ->
          let m = List.map (fun f -> f ()) !cbs_ |> List.flatten in
          Metrics.emit m);
    cbs_ := f :: !cbs_
end

(** {2 Utils} *)

(** Implementation of the W3C Trace Context spec

    https://www.w3.org/TR/trace-context/
*)
module Trace_context = struct
  (** The traceparent header
      https://www.w3.org/TR/trace-context/#traceparent-header
  *)
  module Traceparent = struct
    let name = "traceparent"

    (** Parse the value of the traceparent header.

        The values are of the form:

        {[
        {version}-{trace_id}-{parent_id}-{flags}
        ]}

        For example:

        {[ 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01 ]}

        [{flags}] are currently ignored.
    *)
    let of_value str : (Trace_id.t * Span_id.t, string) result =
      let ( let* ) = result_bind in
      let blit ~offset ~len ~or_ =
        let buf = Bytes.create len in
        let* str =
          match Bytes.blit_string str offset buf 0 len with
          | () -> Ok (Bytes.unsafe_to_string buf)
          | exception Invalid_argument _ -> Error or_
        in
        Ok (str, offset + len)
      in
      let consume expected ~offset ~or_ =
        let len = String.length expected in
        let* str, offset = blit ~offset ~len ~or_ in
        if str = expected then
          Ok offset
        else
          Error or_
      in
      let offset = 0 in
      let* offset = consume "00" ~offset ~or_:"Expected version 00" in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* trace_id, offset =
        blit ~offset ~len:32 ~or_:"Expected 32-digit trace-id"
      in
      let* trace_id =
        match Trace_id.of_hex trace_id with
        | trace_id -> Ok trace_id
        | exception Invalid_argument _ -> Error "Expected hex-encoded trace-id"
      in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* parent_id, offset =
        blit ~offset ~len:16 ~or_:"Expected 16-digit parent-id"
      in
      let* parent_id =
        match Span_id.of_hex parent_id with
        | parent_id -> Ok parent_id
        | exception Invalid_argument _ -> Error "Expected hex-encoded parent-id"
      in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* _flags, _offset =
        blit ~offset ~len:2 ~or_:"Expected 2-digit flags"
      in
      Ok (trace_id, parent_id)

    let to_value ~(trace_id : Trace_id.t) ~(parent_id : Span_id.t) () : string =
      Printf.sprintf "00-%s-%s-00" (Trace_id.to_hex trace_id)
        (Span_id.to_hex parent_id)
  end
end

(** Export GC metrics.

    These metrics are emitted after each GC collection. *)
module GC_metrics : sig
  val basic_setup : unit -> unit
  (** Setup a hook that will emit GC statistics regularly *)

  val get_runtime_attributes : unit -> Span.key_value list
  (** Get OCaml name and version runtime attributes *)

  val get_metrics : unit -> Metrics.t list
  (** Get a few metrics from the current state of the GC *)
end = struct
  (** See https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/resource/semantic_conventions/process.md#process-runtimes *)
  let runtime_attributes =
    lazy
      Conventions.Attributes.
        [
          Process.Runtime.name, `String "ocaml";
          Process.Runtime.version, `String Sys.ocaml_version;
        ]

  let get_runtime_attributes () = Lazy.force runtime_attributes

  let basic_setup () =
    (* emit metrics when GC is called *)
    let on_gc () =
      match Collector.get_backend () with
      | None -> ()
      | Some (module C) -> C.signal_emit_gc_metrics ()
    in
    ignore (Gc.create_alarm on_gc : Gc.alarm)

  let bytes_per_word = Sys.word_size / 8

  let word_to_bytes n = n * bytes_per_word

  let word_to_bytes_f n = n *. float bytes_per_word

  let get_metrics () : Metrics.t list =
    let gc = Gc.quick_stat () in
    let now = Timestamp_ns.now_unix_ns () in
    let open Metrics in
    let open Conventions.Metrics in
    [
      gauge ~name:Process.Runtime.Ocaml.GC.major_heap ~unit_:"B"
        [ int ~now (word_to_bytes gc.Gc.heap_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_allocated
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true ~unit_:"B"
        [ float ~now (word_to_bytes_f gc.Gc.minor_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.minor_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.major_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.major_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.compactions
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.compactions ];
    ]
end
