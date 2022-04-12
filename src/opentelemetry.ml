
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
  type 'msg sender = {
    send: 'a. 'msg -> ret:(unit -> 'a) -> 'a;
  }

  (** Collector client interface. *)
  module type BACKEND = sig
    val send_trace : Trace.resource_spans list sender

    val send_metrics : Metrics.resource_metrics list sender

    val rand_bytes_16 : unit -> bytes
    (** Generate 16 bytes of random data *)

    val rand_bytes_8 : unit -> bytes
    (** Generate 16 bytes of random data *)

    val signal_emit_gc_metrics : unit -> unit
    (** Signal the backend that it should emit GC metrics when it has the
        chance. This should be installed in a GC alarm or another form
        of regular trigger. *)

    val tick : unit -> unit
    (** Should be called regularly for background processing,
        timeout checks, etc. *)

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  let backend : backend option ref = ref None

  (** Is there a configured backend? *)
  let[@inline] has_backend () : bool = !backend != None

  let send_trace (l:Trace.resource_spans list) ~ret =
    match !backend with
    | None -> ret()
    | Some (module B) -> B.send_trace.send l ~ret

  let send_metrics (l:Metrics.resource_metrics list) ~ret =
    match !backend with
    | None -> ret()
    | Some (module B) -> B.send_metrics.send l ~ret

  let rand_bytes_16 () =
    match !backend with
    | None -> Bytes.make 16 '?'
    | Some (module B) -> B.rand_bytes_16()

  let rand_bytes_8 () =
    match !backend with
    | None -> Bytes.make 8 '?'
    | Some (module B) -> B.rand_bytes_8()
end

module Util_ = struct
  let bytes_to_hex (b:bytes) : string =
    let i_to_hex (i:int) =
      if i < 10 then Char.chr (i + Char.code '0')
      else Char.chr (i - 10 + Char.code 'a')
    in

    let res = Bytes.create (2 * Bytes.length b) in
    for i = 0 to Bytes.length b-1 do
      let n = Char.code (Bytes.get b i) in
      Bytes.set res (2 * i) (i_to_hex ((n land 0xf0) lsr 4));
      Bytes.set res (2 * i + 1) (i_to_hex (n land 0x0f));
    done;
    Bytes.unsafe_to_string res

  let bytes_of_hex (s:string) : bytes =
    let n_of_c = function
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
      | _ -> raise (Invalid_argument "invalid hex char")
    in
    if (String.length s mod 2 <> 0) then raise (Invalid_argument "hex sequence must be of even length");
    let res = Bytes.make (String.length s / 2) '\x00' in
    for i=0 to String.length s/2-1 do
      let n1 = n_of_c (String.get s (2*i)) in
      let n2 = n_of_c (String.get s (2*i+1)) in
      let n = (n1 lsl 4) lor n2 in
      Bytes.set res i (Char.chr n)
    done;
    res
end

(** Trace ID.

    This 16 bytes identifier is shared by all spans in one trace. *)
module Trace_id : sig
  type t
  val create : unit -> t
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
  val to_hex : t -> string
  val of_hex : string -> t
end = struct
  open Proto.Trace
  type t = bytes
  let to_bytes self = self
  let create () : t = Collector.rand_bytes_16()
  let of_bytes b = if Bytes.length b=16 then b else raise (Invalid_argument "trace IDs must be 16 bytes in length")
  let to_hex self = Util_.bytes_to_hex self
  let of_hex s = of_bytes (Util_.bytes_of_hex s)
end

(** Unique ID of a span. *)
module Span_id : sig
  type t
  val create : unit -> t
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
  val to_hex : t -> string
  val of_hex : string -> t
end = struct
  open Proto.Trace
  type t = bytes
  let to_bytes self = self
  let create () : t = Collector.rand_bytes_8()
  let of_bytes b = if Bytes.length b=8 then b else raise (Invalid_argument "span IDs must be 8 bytes in length")
  let to_hex self = Util_.bytes_to_hex self
  let of_hex s = of_bytes (Util_.bytes_of_hex s)
end

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

type value = [`Int of int | `String of string | `Bool of bool | `None]

type key_value = string * value

(**/**)
let _conv_value =
  let open Proto.Common in
  function
  | `Int i -> Some (Int_value (Int64.of_int i))
  | `String s -> Some (String_value s)
  | `Bool b -> Some (Bool_value b)
  | `None -> None

(**/**)

(**/**)
let _conv_key_value (k,v) =
  let open Proto.Common in
  let value = _conv_value v in
  default_key_value ~key:k ~value ()

(**/**)

(** Process-wide metadata, environment variables, etc. *)
module Globals = struct
  open Proto.Common

  let service_name = ref "unknown_service"
  (** Main service name metadata *)

  let service_namespace = ref None
  (** Namespace for the service *)

  let instrumentation_library =
    default_instrumentation_library
      ~version:"%%VERSION%%"
      ~name:"ocaml-opentelemetry" ()

  (** Global attributes, set via OTEL_RESOURCE_ATTRIBUTES *)
  let global_attributes : key_value list =
    let parse_pair s = match String.split_on_char '=' s with
      | [a;b] -> default_key_value ~key:a  ~value:(Some (String_value b)) ()
      | _ -> failwith (Printf.sprintf "invalid attribute: %S" s)
    in
    try
      Sys.getenv "OTEL_RESOURCE_ATTRIBUTES" |> String.split_on_char ','
      |> List.map parse_pair
    with _ -> []

  (* add global attributes to this list *)
  let merge_global_attributes_ into : _ list =
    let not_redundant kv = List.for_all (fun kv' -> kv.key <> kv'.key) into in
    List.rev_append (List.filter not_redundant global_attributes) into

  let mk_attributes ?(service_name = !service_name) ?(attrs=[]) () : _ list =
    let l = List.map _conv_key_value attrs in
    let l =
      default_key_value ~key:"service.name"
        ~value:(Some (String_value service_name)) () :: l
    in
    let l = match !service_namespace with
      | None -> l
      | Some v ->
        default_key_value ~key:"service.name"
          ~value:(Some (String_value v)) () :: l
    in
    l |> merge_global_attributes_
end

(** Events.

    Events occur at a given time and can carry attributes. They always
    belong in a span. *)
module Event : sig
  open Proto.Trace
  type t = span_event

  val make :
    ?time_unix_nano:Timestamp_ns.t ->
    ?attrs:key_value list ->
    string ->
    t

end = struct
  open Proto.Trace
  type t = span_event

  let make
      ?(time_unix_nano=Timestamp_ns.now_unix_ns())
      ?(attrs=[])
      (name:string) : t =
    let attrs = List.map _conv_key_value attrs in
    default_span_event ~time_unix_nano ~name ~attributes:attrs ()
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

  type key_value = string * [`Int of int | `String of string | `Bool of bool | `None]

  val create :
    ?kind:kind ->
    ?id:id ->
    ?trace_state:string ->
    ?attrs:key_value list ->
    ?events:Event.t list ->
    ?status:status ->
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

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type key_value = string * [`Int of int | `String of string | `Bool of bool | `None]

  type nonrec status_code = status_status_code =
    | Status_code_unset
    | Status_code_ok
    | Status_code_error

  type nonrec status = status = {
    message: string;
    code: status_code;
  }


  let id self = Span_id.of_bytes self.span_id

  let create
      ?(kind=Span_kind_unspecified)
      ?(id=Span_id.create())
      ?trace_state
      ?(attrs=[])
      ?(events=[])
      ?status
      ~trace_id ?parent ?(links=[])
      ~start_time ~end_time
      name : t * id =
    let trace_id = Trace_id.to_bytes trace_id in
    let parent_span_id = Option.map Span_id.to_bytes parent in
    let attributes = List.map _conv_key_value attrs in
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
        ~attributes ~events
        ?trace_state ~status
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

  let make_resource_spans ?service_name ?attrs spans =
    let ils =
      default_instrumentation_library_spans
        ~instrumentation_library:(Some Globals.instrumentation_library)
        ~spans () in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_spans
      ~resource:(Some resource) ~instrumentation_library_spans:[ils] ()

  (** Sync emitter *)
  let emit ?service_name ?attrs (spans:span list) : unit =
    let rs = make_resource_spans ?service_name ?attrs spans in
    Collector.send_trace [rs] ~ret:(fun () -> ())

  (** Scope to be used with {!with_}. *)
  type scope = {
    trace_id: Trace_id.t;
    span_id: Span_id.t;
    mutable events: Event.t list;
    mutable attrs: Span.key_value list
  }

  (** Add an event to the scope. It will be aggregated into the span.

      Note that this takes a function that produces an event, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_event (scope:scope) (ev:unit -> Event.t) : unit =
    if Collector.has_backend() then (
      scope.events <- ev() :: scope.events
    )

  (** Add an attr to the scope. It will be aggregated into the span.

      Note that this takes a function that produces attributes, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_attrs (scope:scope) (attrs:unit -> Span.key_value list) : unit =
    if Collector.has_backend() then (
      scope.attrs <- List.rev_append (attrs ()) scope.attrs
    )

  (** Sync span guard *)
  let with_
      ?trace_state ?service_name ?(attrs: (string*[<value]) list = [])
      ?kind ?trace_id ?parent ?scope ?links
      name (f: scope -> 'a) : 'a =
    let trace_id =
      match trace_id, scope with
      | Some trace_id, _ -> trace_id
      | None, Some scope -> scope.trace_id
      | None, None -> Trace_id.create ()
    in
    let parent =
      match parent, scope with
      | Some span_id, _ -> Some span_id
      | None, Some scope -> Some scope.span_id
      | None, None -> None
    in
    let start_time = Timestamp_ns.now_unix_ns() in
    let span_id = Span_id.create() in
    let scope = {trace_id;span_id;events=[]; attrs} in

    (* called once we're done, to emit a span *)
    let finally res =
      let status = match res with
        | Ok () -> default_status ~code:Status_code_ok ()
        | Error e -> default_status ~code:Status_code_error ~message:e () in
      let span, _ =
        (* TODO: should the attrs passed to with_ go on the Span (in Span.create) or on the ResourceSpan (in emit)?
           (question also applies to Opentelemetry_lwt.Trace.with) *)
        Span.create
          ?kind ~trace_id ?parent ?links ~id:span_id
          ?trace_state ~attrs:scope.attrs ~events:scope.events
          ~start_time ~end_time:(Timestamp_ns.now_unix_ns())
          ~status
          name in
      emit ?service_name [span];
    in
    try
      let x = f scope in
      finally (Ok ());
      x
    with e ->
      finally (Error (Printexc.to_string e));
      raise e
end

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  open Metrics_types

  type t = Metrics_types.metric

  (** Number data point, as a float *)
  let float ?start_time_unix_nano
      ?(now=Timestamp_ns.now_unix_ns())
      ?(attrs=[])
      (d:float) : number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point
      ?start_time_unix_nano ~time_unix_nano:now
      ~attributes
      ~value:(As_double d) ()

  (** Number data point, as an int *)
  let int ?start_time_unix_nano
      ?(now=Timestamp_ns.now_unix_ns())
      ?(attrs=[])
      (i:int) : number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point ?start_time_unix_nano ~time_unix_nano:now
      ~attributes
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
      ?(aggregation_temporality=Aggregation_temporality_cumulative)
      ?is_monotonic
      (l:number_data_point list) : t =
    let data =
      Sum (default_sum ~data_points:l ?is_monotonic
             ~aggregation_temporality ()) in
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

  (** Aggregate metrics into a {!Proto.Metrics.resource_metrics} *)
  let make_resource_metrics ?service_name ?attrs (l:t list) : resource_metrics =
    let lm =
      default_instrumentation_library_metrics
        ~instrumentation_library:(Some Globals.instrumentation_library)
        ~metrics:l () in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_metrics
      ~instrumentation_library_metrics:[lm] ~resource:(Some resource) ()

  (** Emit some metrics to the collector (sync). This blocks until
      the backend has pushed the metrics into some internal queue, or
      discarded them. *)
  let emit ?attrs (l:t list) : unit =
    let rm = make_resource_metrics ?attrs l in
    Collector.send_metrics [rm] ~ret:ignore
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
      let ( let* ) = Result.bind in
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
        if str = expected then Ok offset else Error or_
      in
      let offset = 0 in
      let* offset = consume "00" ~offset ~or_:"Expected version 00" in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* trace_id, offset = blit ~offset ~len:32 ~or_:"Expected 32-digit trace-id" in
      let* trace_id =
        match Trace_id.of_hex trace_id with
        | trace_id -> Ok trace_id
        | exception Invalid_argument _ -> Error "Expected hex-encoded trace-id"
      in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* parent_id, offset = blit ~offset ~len:16 ~or_:"Expected 16-digit parent-id" in
      let* parent_id =
        match Span_id.of_hex parent_id with
        | parent_id -> Ok parent_id
        | exception Invalid_argument _ -> Error "Expected hex-encoded parent-id"
      in
      let* offset = consume "-" ~offset ~or_:"Expected delimiter" in
      let* _flags, _offset = blit ~offset ~len:2 ~or_:"Expected 2-digit flags" in
      Ok (trace_id, parent_id)

    let to_value ~(trace_id : Trace_id.t) ~(parent_id : Span_id.t) () : string =
      Printf.sprintf "00-%s-%s-00"
        (Trace_id.to_hex trace_id)
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
      Conventions.Attributes.[
         (Process.Runtime.name, `String "ocaml");
         (Process.Runtime.version, `String Sys.ocaml_version);
       ]

  let get_runtime_attributes () = Lazy.force runtime_attributes

  let basic_setup () =
    let trigger() =
      match !Collector.backend with
      | None -> ()
      | Some (module C) -> C.signal_emit_gc_metrics()
    in
    ignore (Gc.create_alarm trigger : Gc.alarm)

  let bytes_per_word = Sys.word_size / 8
  let word_to_bytes n = n * bytes_per_word
  let word_to_bytes_f n = n *. float bytes_per_word

  (* TODO: use atomic *)
  let last = ref (Timestamp_ns.now_unix_ns())

  let get_metrics () : Metrics.t list =
    let gc = Gc.quick_stat () in
    let start_time_unix_nano = !last in
    last := Timestamp_ns.now_unix_ns();
    let open Metrics in
    let open Conventions.Metrics in
    [
      gauge ~name:Process.Runtime.Ocaml.GC.major_heap ~unit_:"B"
        [ int (word_to_bytes gc.Gc.heap_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_allocated
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        ~unit_:"B"
        [ float ~start_time_unix_nano (word_to_bytes_f gc.Gc.minor_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~start_time_unix_nano gc.Gc.minor_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.major_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~start_time_unix_nano gc.Gc.major_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.compactions
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~start_time_unix_nano gc.Gc.compactions ];
    ]
end
