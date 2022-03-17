
(** Protobuf types *)
module Proto = struct
  module Common = struct
    include Common_types
    include Common_pp
    include Common_pb
  end

  module Resource = struct
    include Resource_types
    include Resource_pp
    include Resource_pb
  end

  module Trace = struct
    include Trace_types
    include Trace_pp
    include Trace_pb
  end

  module Metrics = struct
    include Metrics_types
    include Metrics_pp
    include Metrics_pb
  end

  module Trace_service = struct
    include Trace_service_types
    include Trace_service_pb
    include Trace_service_pp
  end

  module Metrics_service = struct
    include Metrics_service_types
    include Metrics_service_pp
    include Metrics_service_pb
  end

  module Status = struct
    include Status_types
    include Status_pp
    include Status_pb
  end
end

(** Unix timestamp.

    These timestamps measure time since the Unix epoch (jan 1, 1970) UTC
    in nanoseconds. *)
module Timestamp_ns = struct
  type t = int64
  let ns_in_a_day = Int64.(mul 1_000_000_000L (of_int (24 * 3600)))

  (** Current unix timestamp in nanoseconds *)
  let[@inline] now_unix_ns () : t =
    let span = Ptime_clock.now() |> Ptime.to_span in
    let d, ps = Ptime.Span.to_d_ps span in
    let d = Int64.(mul (of_int d) ns_in_a_day) in
    let ns = Int64.(div ps 1_000L) in
    Int64.(add d ns)
end

(** Collector types

    These types are used by backend implementations, to send events to
    collectors such as Jaeger.

    Note: most users will not need to touch this module *)
module Collector = struct
  open Proto

  (** Collector client interface. *)
  module type BACKEND = sig
    val send_trace : Trace_service.export_trace_service_request -> unit

    val send_metrics : Metrics_service.export_metrics_service_request -> unit

    val rand_bytes_16 : unit -> bytes
    (** Generate 16 bytes of random data *)

    val rand_bytes_8 : unit -> bytes
    (** Generate 16 bytes of random data *)

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  let backend : backend option ref = ref None

  let send_trace (l:Trace.resource_spans list) : unit =
    match !backend with
    | None -> ()
    | Some (module B) ->
      let ev = Trace_service.default_export_trace_service_request
          ~resource_spans:l () in
      B.send_trace ev

  let send_metrics (l:Metrics.resource_metrics list) : unit =
    match !backend with
    | None -> ()
    | Some (module B) ->
      let ev = Metrics_service.default_export_metrics_service_request
          ~resource_metrics:l () in
      B.send_metrics ev

  let rand_bytes_16 () =
    match !backend with
    | None -> Bytes.make 16 '?'
    | Some (module B) -> B.rand_bytes_16()

  let rand_bytes_8 () =
    match !backend with
    | None -> Bytes.make 8 '?'
    | Some (module B) -> B.rand_bytes_8()
end

(** Trace ID.

    This 16 bytes identifier is shared by all spans in one trace. *)
module Trace_id : sig
  type t
  val create : unit -> t
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
end = struct
  open Proto.Trace
  type t = bytes
  let to_bytes self = self
  let create () : t = Collector.rand_bytes_16()
  let of_bytes b = assert(Bytes.length b=16); b
end

(** Unique ID of a span. *)
module Span_id : sig
  type t
  val create : unit -> t
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
end = struct
  open Proto.Trace
  type t = bytes
  let to_bytes self = self
  let create () : t = Collector.rand_bytes_8()
  let of_bytes b = assert(Bytes.length b=8); b
end


(* TODO: Event.t, use it in Span *)

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

  val id : t -> Span_id.t

  type key_value = string * [`Int of int | `String of string | `Bool of bool | `None]

  val create :
    ?kind:kind ->
    ?id:id ->
    ?trace_state:string ->
    ?service_name:string ->
    ?attrs:key_value list ->
    trace_id:Trace_id.t ->
    ?parent:id ->
    ?links:(Trace_id.t * Span_id.t * string) list ->
    start_time:Timestamp_ns.t ->
    end_time:Timestamp_ns.t ->
    string -> t * id
    (** [create ~trace_id name] creates a new span with its unique ID.
        @param trace_id the trace this belongs to
        @param parent parent span, if any
        @param links list of links to other spans, each with their trace state
        (see {{: https://www.w3.org/TR/trace-context/#tracestate-header} w3.org}) *)
end = struct
  open Proto.Trace

  type t = span
  type id = Span_id.t

  let id self = Span_id.of_bytes self.span_id

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type key_value = string * [`Int of int | `String of string | `Bool of bool | `None]

  let create
      ?(kind=Span_kind_unspecified)
      ?(id=Span_id.create())
      ?trace_state
      ?service_name
      ?(attrs=[])
      ~trace_id ?parent ?(links=[])
      ~start_time ~end_time
      name : t * id =
    let trace_id = Trace_id.to_bytes trace_id in
    let parent_span_id = Option.map Span_id.to_bytes parent in

    let attributes =
      let open Proto.Common in
      let l = List.map
          (fun (k,v) ->
             let value = match v with
               | `Int i -> Some (Int_value (Int64.of_int i))
               | `String s -> Some (String_value s)
               | `Bool b -> Some (Bool_value b)
               | `None -> None
             in
             default_key_value ~key:k ~value ())
          attrs
      in
      let l = match service_name with
        | None -> l
        | Some v ->
          default_key_value ~key:"service.name"
            ~value:(Some (String_value v)) () :: l
      in
      l
    in

    let links =
      List.map
        (fun (trace_id,span_id,trace_state) ->
           let trace_id = Trace_id.to_bytes trace_id in
           let span_id = Span_id.to_bytes span_id in
           default_span_link ~trace_id ~span_id ~trace_state())
        links
    in
    let span =
      default_span
        ~trace_id ?parent_span_id
        ~span_id:(Span_id.to_bytes id)
        ~attributes
        ?trace_state
        ~kind ~name ~links
        ~start_time_unix_nano:start_time
        ~end_time_unix_nano:end_time
        ()
    in
    span, id
end

(** Traces.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal} the spec} *)
module Trace = struct
  open Proto.Trace

  type span = Span.t

  let emit (spans:span list) : unit =
    let ils =
      default_instrumentation_library_spans ~spans () in
    let rs = default_resource_spans ~instrumentation_library_spans:[ils] () in
    Collector.send_trace [rs]

  let with_
      ?trace_state ?service_name ?attrs
      ?kind ?(trace_id=Trace_id.create()) ?parent ?links
      name (f:Trace_id.t * Span_id.t -> 'a) : 'a =
    let start_time = Timestamp_ns.now_unix_ns() in
    let span_id = Span_id.create() in
    let finally() =
      let span, _ =
        Span.create
          ?kind ~trace_id ?parent ?links ~id:span_id
          ?trace_state ?service_name ?attrs
          ~start_time ~end_time:(Timestamp_ns.now_unix_ns())
          name in
      emit [span];
    in
    Fun.protect ~finally (fun () -> f (trace_id,span_id))
end

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  open Metrics_types

  type t = Metrics_types.metric

  (** Number data point, as a float *)
  let float ?start_time_unix_nano
      ?(now=Timestamp_ns.now_unix_ns())
      (d:float) : number_data_point =
    default_number_data_point ?start_time_unix_nano ~time_unix_nano:now
      ~value:(As_double d) ()

  (** Number data point, as an int *)
  let int ?start_time_unix_nano
      ?(now=Timestamp_ns.now_unix_ns())
      (i:int) : number_data_point =
    default_number_data_point ?start_time_unix_nano ~time_unix_nano:now
      ~value:(As_int (Int64.of_int i)) ()

  (** Aggregation of a scalar metric, always with the current value *)
  let gauge ~name ?description ?unit_ (l:number_data_point list) : t =
    let data = Gauge (default_gauge ~data_points:l ()) in
    default_metric ~name ?description ?unit_ ~data ()

  type aggregation_temporality = Metrics_types.aggregation_temporality =
    | Aggregation_temporality_unspecified
    | Aggregation_temporality_delta
    | Aggregation_temporality_cumulative

  (** Sum of all reported measurements over a time interval *)
  let sum ~name ?description ?unit_
      ?aggregation_temporality ?is_monotonic
      (l:number_data_point list) : t =
    let data =
      Sum (default_sum ~data_points:l ?is_monotonic
             ?aggregation_temporality ()) in
    default_metric ~name ?description ?unit_ ~data ()

  (* TODO
  let histogram ~name ?description ?unit_
      ?aggregation_temporality
      (l:number_data_point list) : t =
    let data h=
      Histogram (default_histogram ~data_points:l
             ?aggregation_temporality ()) in
    default_metric ~name ?description ?unit_ ~data ()
     *)

  (* TODO: exponential history *)
  (* TODO: summary *)
  (* TODO: exemplar *)

  (** Emit a bunch of metrics to the collector. *)
  let emit (l:t list) : unit =
    let lm =
      default_instrumentation_library_metrics ~metrics:l () in
    let rm = default_resource_metrics
        ~instrumentation_library_metrics:[lm] () in
    Collector.send_metrics [rm]
end

(*
module Span = Span
module Timestamp = Timestamp
   *)

