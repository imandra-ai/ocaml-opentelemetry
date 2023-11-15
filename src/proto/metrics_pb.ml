[@@@ocaml.warning "-27-30-39"]

type exemplar_mutable = {
  mutable filtered_attributes : Common_types.key_value list;
  mutable time_unix_nano : int64;
  mutable value : Metrics_types.exemplar_value;
  mutable span_id : bytes;
  mutable trace_id : bytes;
}

let default_exemplar_mutable () : exemplar_mutable = {
  filtered_attributes = [];
  time_unix_nano = 0L;
  value = Metrics_types.As_double (0.);
  span_id = Bytes.create 0;
  trace_id = Bytes.create 0;
}

type number_data_point_mutable = {
  mutable attributes : Common_types.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable value : Metrics_types.number_data_point_value;
  mutable exemplars : Metrics_types.exemplar list;
  mutable flags : int32;
}

let default_number_data_point_mutable () : number_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  value = Metrics_types.As_double (0.);
  exemplars = [];
  flags = 0l;
}

type gauge_mutable = {
  mutable data_points : Metrics_types.number_data_point list;
}

let default_gauge_mutable () : gauge_mutable = {
  data_points = [];
}

type sum_mutable = {
  mutable data_points : Metrics_types.number_data_point list;
  mutable aggregation_temporality : Metrics_types.aggregation_temporality;
  mutable is_monotonic : bool;
}

let default_sum_mutable () : sum_mutable = {
  data_points = [];
  aggregation_temporality = Metrics_types.default_aggregation_temporality ();
  is_monotonic = false;
}

type histogram_data_point_mutable = {
  mutable attributes : Common_types.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable bucket_counts : int64 list;
  mutable explicit_bounds : float list;
  mutable exemplars : Metrics_types.exemplar list;
  mutable flags : int32;
  mutable min : float option;
  mutable max : float option;
}

let default_histogram_data_point_mutable () : histogram_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = None;
  bucket_counts = [];
  explicit_bounds = [];
  exemplars = [];
  flags = 0l;
  min = None;
  max = None;
}

type histogram_mutable = {
  mutable data_points : Metrics_types.histogram_data_point list;
  mutable aggregation_temporality : Metrics_types.aggregation_temporality;
}

let default_histogram_mutable () : histogram_mutable = {
  data_points = [];
  aggregation_temporality = Metrics_types.default_aggregation_temporality ();
}

type exponential_histogram_data_point_buckets_mutable = {
  mutable offset : int32;
  mutable bucket_counts : int64 list;
}

let default_exponential_histogram_data_point_buckets_mutable () : exponential_histogram_data_point_buckets_mutable = {
  offset = 0l;
  bucket_counts = [];
}

type exponential_histogram_data_point_mutable = {
  mutable attributes : Common_types.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable scale : int32;
  mutable zero_count : int64;
  mutable positive : Metrics_types.exponential_histogram_data_point_buckets option;
  mutable negative : Metrics_types.exponential_histogram_data_point_buckets option;
  mutable flags : int32;
  mutable exemplars : Metrics_types.exemplar list;
  mutable min : float option;
  mutable max : float option;
  mutable zero_threshold : float;
}

let default_exponential_histogram_data_point_mutable () : exponential_histogram_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = None;
  scale = 0l;
  zero_count = 0L;
  positive = None;
  negative = None;
  flags = 0l;
  exemplars = [];
  min = None;
  max = None;
  zero_threshold = 0.;
}

type exponential_histogram_mutable = {
  mutable data_points : Metrics_types.exponential_histogram_data_point list;
  mutable aggregation_temporality : Metrics_types.aggregation_temporality;
}

let default_exponential_histogram_mutable () : exponential_histogram_mutable = {
  data_points = [];
  aggregation_temporality = Metrics_types.default_aggregation_temporality ();
}

type summary_data_point_value_at_quantile_mutable = {
  mutable quantile : float;
  mutable value : float;
}

let default_summary_data_point_value_at_quantile_mutable () : summary_data_point_value_at_quantile_mutable = {
  quantile = 0.;
  value = 0.;
}

type summary_data_point_mutable = {
  mutable attributes : Common_types.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable quantile_values : Metrics_types.summary_data_point_value_at_quantile list;
  mutable flags : int32;
}

let default_summary_data_point_mutable () : summary_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = 0.;
  quantile_values = [];
  flags = 0l;
}

type summary_mutable = {
  mutable data_points : Metrics_types.summary_data_point list;
}

let default_summary_mutable () : summary_mutable = {
  data_points = [];
}

type metric_mutable = {
  mutable name : string;
  mutable description : string;
  mutable unit_ : string;
  mutable data : Metrics_types.metric_data;
}

let default_metric_mutable () : metric_mutable = {
  name = "";
  description = "";
  unit_ = "";
  data = Metrics_types.Gauge (Metrics_types.default_gauge ());
}

type scope_metrics_mutable = {
  mutable scope : Common_types.instrumentation_scope option;
  mutable metrics : Metrics_types.metric list;
  mutable schema_url : string;
}

let default_scope_metrics_mutable () : scope_metrics_mutable = {
  scope = None;
  metrics = [];
  schema_url = "";
}

type resource_metrics_mutable = {
  mutable resource : Resource_types.resource option;
  mutable scope_metrics : Metrics_types.scope_metrics list;
  mutable schema_url : string;
}

let default_resource_metrics_mutable () : resource_metrics_mutable = {
  resource = None;
  scope_metrics = [];
  schema_url = "";
}

type metrics_data_mutable = {
  mutable resource_metrics : Metrics_types.resource_metrics list;
}

let default_metrics_data_mutable () : metrics_data_mutable = {
  resource_metrics = [];
}


let rec decode_exemplar_value d = 
  let rec loop () = 
    let ret:Metrics_types.exemplar_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "exemplar_value"
      | Some (3, _) -> (Metrics_types.As_double (Pbrt.Decoder.float_as_bits64 d) : Metrics_types.exemplar_value) 
      | Some (6, _) -> (Metrics_types.As_int (Pbrt.Decoder.int64_as_bits64 d) : Metrics_types.exemplar_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_exemplar d =
  let v = default_exemplar_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.filtered_attributes <- List.rev v.filtered_attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.filtered_attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.filtered_attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.value <- Metrics_types.As_double (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(3)" pk
    | Some (6, Pbrt.Bits64) -> begin
      v.value <- Metrics_types.As_int (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(6)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.filtered_attributes = v.filtered_attributes;
    Metrics_types.time_unix_nano = v.time_unix_nano;
    Metrics_types.value = v.value;
    Metrics_types.span_id = v.span_id;
    Metrics_types.trace_id = v.trace_id;
  } : Metrics_types.exemplar)

let rec decode_number_data_point_value d = 
  let rec loop () = 
    let ret:Metrics_types.number_data_point_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "number_data_point_value"
      | Some (4, _) -> (Metrics_types.As_double (Pbrt.Decoder.float_as_bits64 d) : Metrics_types.number_data_point_value) 
      | Some (6, _) -> (Metrics_types.As_int (Pbrt.Decoder.int64_as_bits64 d) : Metrics_types.number_data_point_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_number_data_point d =
  let v = default_number_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.value <- Metrics_types.As_double (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(4)" pk
    | Some (6, Pbrt.Bits64) -> begin
      v.value <- Metrics_types.As_int (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(6)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(5)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.attributes = v.attributes;
    Metrics_types.start_time_unix_nano = v.start_time_unix_nano;
    Metrics_types.time_unix_nano = v.time_unix_nano;
    Metrics_types.value = v.value;
    Metrics_types.exemplars = v.exemplars;
    Metrics_types.flags = v.flags;
  } : Metrics_types.number_data_point)

let rec decode_gauge d =
  let v = default_gauge_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(gauge), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.data_points = v.data_points;
  } : Metrics_types.gauge)

let rec decode_aggregation_temporality d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Metrics_types.Aggregation_temporality_unspecified:Metrics_types.aggregation_temporality)
  | 1 -> (Metrics_types.Aggregation_temporality_delta:Metrics_types.aggregation_temporality)
  | 2 -> (Metrics_types.Aggregation_temporality_cumulative:Metrics_types.aggregation_temporality)
  | _ -> Pbrt.Decoder.malformed_variant "aggregation_temporality"

let rec decode_sum d =
  let v = default_sum_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.is_monotonic <- Pbrt.Decoder.bool d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.data_points = v.data_points;
    Metrics_types.aggregation_temporality = v.aggregation_temporality;
    Metrics_types.is_monotonic = v.is_monotonic;
  } : Metrics_types.sum)

let rec decode_histogram_data_point d =
  let v = default_histogram_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.explicit_bounds <- List.rev v.explicit_bounds;
      v.bucket_counts <- List.rev v.bucket_counts;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (9, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(9)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.bucket_counts <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_bits64 d)::l) [] d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.explicit_bounds <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(8)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(10)" pk
    | Some (11, Pbrt.Bits64) -> begin
      v.min <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(11)" pk
    | Some (12, Pbrt.Bits64) -> begin
      v.max <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(12)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.attributes = v.attributes;
    Metrics_types.start_time_unix_nano = v.start_time_unix_nano;
    Metrics_types.time_unix_nano = v.time_unix_nano;
    Metrics_types.count = v.count;
    Metrics_types.sum = v.sum;
    Metrics_types.bucket_counts = v.bucket_counts;
    Metrics_types.explicit_bounds = v.explicit_bounds;
    Metrics_types.exemplars = v.exemplars;
    Metrics_types.flags = v.flags;
    Metrics_types.min = v.min;
    Metrics_types.max = v.max;
  } : Metrics_types.histogram_data_point)

let rec decode_histogram d =
  let v = default_histogram_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.data_points = v.data_points;
    Metrics_types.aggregation_temporality = v.aggregation_temporality;
  } : Metrics_types.histogram)

let rec decode_exponential_histogram_data_point_buckets d =
  let v = default_exponential_histogram_data_point_buckets_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.bucket_counts <- List.rev v.bucket_counts;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.offset <- Pbrt.Decoder.int32_as_zigzag d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point_buckets), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.bucket_counts <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point_buckets), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.offset = v.offset;
    Metrics_types.bucket_counts = v.bucket_counts;
  } : Metrics_types.exponential_histogram_data_point_buckets)

let rec decode_exponential_histogram_data_point d =
  let v = default_exponential_histogram_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(1)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.scale <- Pbrt.Decoder.int32_as_zigzag d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(6)" pk
    | Some (7, Pbrt.Bits64) -> begin
      v.zero_count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.positive <- Some (decode_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.negative <- Some (decode_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(11)" pk
    | Some (12, Pbrt.Bits64) -> begin
      v.min <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(12)" pk
    | Some (13, Pbrt.Bits64) -> begin
      v.max <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(13)" pk
    | Some (14, Pbrt.Bits64) -> begin
      v.zero_threshold <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(14)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.attributes = v.attributes;
    Metrics_types.start_time_unix_nano = v.start_time_unix_nano;
    Metrics_types.time_unix_nano = v.time_unix_nano;
    Metrics_types.count = v.count;
    Metrics_types.sum = v.sum;
    Metrics_types.scale = v.scale;
    Metrics_types.zero_count = v.zero_count;
    Metrics_types.positive = v.positive;
    Metrics_types.negative = v.negative;
    Metrics_types.flags = v.flags;
    Metrics_types.exemplars = v.exemplars;
    Metrics_types.min = v.min;
    Metrics_types.max = v.max;
    Metrics_types.zero_threshold = v.zero_threshold;
  } : Metrics_types.exponential_histogram_data_point)

let rec decode_exponential_histogram d =
  let v = default_exponential_histogram_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_exponential_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.data_points = v.data_points;
    Metrics_types.aggregation_temporality = v.aggregation_temporality;
  } : Metrics_types.exponential_histogram)

let rec decode_summary_data_point_value_at_quantile d =
  let v = default_summary_data_point_value_at_quantile_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.quantile <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point_value_at_quantile), field(1)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.value <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point_value_at_quantile), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.quantile = v.quantile;
    Metrics_types.value = v.value;
  } : Metrics_types.summary_data_point_value_at_quantile)

let rec decode_summary_data_point d =
  let v = default_summary_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.quantile_values <- List.rev v.quantile_values;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.quantile_values <- (decode_summary_data_point_value_at_quantile (Pbrt.Decoder.nested d)) :: v.quantile_values;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(6)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.attributes = v.attributes;
    Metrics_types.start_time_unix_nano = v.start_time_unix_nano;
    Metrics_types.time_unix_nano = v.time_unix_nano;
    Metrics_types.count = v.count;
    Metrics_types.sum = v.sum;
    Metrics_types.quantile_values = v.quantile_values;
    Metrics_types.flags = v.flags;
  } : Metrics_types.summary_data_point)

let rec decode_summary d =
  let v = default_summary_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_summary_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.data_points = v.data_points;
  } : Metrics_types.summary)

let rec decode_metric_data d = 
  let rec loop () = 
    let ret:Metrics_types.metric_data = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "metric_data"
      | Some (5, _) -> (Metrics_types.Gauge (decode_gauge (Pbrt.Decoder.nested d)) : Metrics_types.metric_data) 
      | Some (7, _) -> (Metrics_types.Sum (decode_sum (Pbrt.Decoder.nested d)) : Metrics_types.metric_data) 
      | Some (9, _) -> (Metrics_types.Histogram (decode_histogram (Pbrt.Decoder.nested d)) : Metrics_types.metric_data) 
      | Some (10, _) -> (Metrics_types.Exponential_histogram (decode_exponential_histogram (Pbrt.Decoder.nested d)) : Metrics_types.metric_data) 
      | Some (11, _) -> (Metrics_types.Summary (decode_summary (Pbrt.Decoder.nested d)) : Metrics_types.metric_data) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_metric d =
  let v = default_metric_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.description <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.unit_ <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.data <- Metrics_types.Gauge (decode_gauge (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(5)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.data <- Metrics_types.Sum (decode_sum (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(7)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.data <- Metrics_types.Histogram (decode_histogram (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.data <- Metrics_types.Exponential_histogram (decode_exponential_histogram (Pbrt.Decoder.nested d));
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.data <- Metrics_types.Summary (decode_summary (Pbrt.Decoder.nested d));
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(11)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.name = v.name;
    Metrics_types.description = v.description;
    Metrics_types.unit_ = v.unit_;
    Metrics_types.data = v.data;
  } : Metrics_types.metric)

let rec decode_scope_metrics d =
  let v = default_scope_metrics_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.metrics <- List.rev v.metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.scope <- Some (Common_pb.decode_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.metrics <- (decode_metric (Pbrt.Decoder.nested d)) :: v.metrics;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.scope = v.scope;
    Metrics_types.metrics = v.metrics;
    Metrics_types.schema_url = v.schema_url;
  } : Metrics_types.scope_metrics)

let rec decode_resource_metrics d =
  let v = default_resource_metrics_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.scope_metrics <- List.rev v.scope_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource_pb.decode_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.scope_metrics <- (decode_scope_metrics (Pbrt.Decoder.nested d)) :: v.scope_metrics;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.resource = v.resource;
    Metrics_types.scope_metrics = v.scope_metrics;
    Metrics_types.schema_url = v.schema_url;
  } : Metrics_types.resource_metrics)

let rec decode_metrics_data d =
  let v = default_metrics_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_metrics <- List.rev v.resource_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_metrics <- (decode_resource_metrics (Pbrt.Decoder.nested d)) :: v.resource_metrics;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metrics_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_types.resource_metrics = v.resource_metrics;
  } : Metrics_types.metrics_data)

let rec decode_data_point_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Metrics_types.Data_point_flags_do_not_use:Metrics_types.data_point_flags)
  | 1 -> (Metrics_types.Data_point_flags_no_recorded_value_mask:Metrics_types.data_point_flags)
  | _ -> Pbrt.Decoder.malformed_variant "data_point_flags"

let rec encode_exemplar_value (v:Metrics_types.exemplar_value) encoder = 
  begin match v with
  | Metrics_types.As_double x ->
    Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | Metrics_types.As_int x ->
    Pbrt.Encoder.key (6, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.int64_as_bits64 x encoder;
  end

and encode_exemplar (v:Metrics_types.exemplar) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Metrics_types.filtered_attributes;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.time_unix_nano encoder;
  begin match v.Metrics_types.value with
  | Metrics_types.As_double x ->
    Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | Metrics_types.As_int x ->
    Pbrt.Encoder.key (6, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.int64_as_bits64 x encoder;
  end;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Metrics_types.span_id encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Metrics_types.trace_id encoder;
  ()

let rec encode_number_data_point_value (v:Metrics_types.number_data_point_value) encoder = 
  begin match v with
  | Metrics_types.As_double x ->
    Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | Metrics_types.As_int x ->
    Pbrt.Encoder.key (6, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.int64_as_bits64 x encoder;
  end

and encode_number_data_point (v:Metrics_types.number_data_point) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Metrics_types.attributes;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.start_time_unix_nano encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.time_unix_nano encoder;
  begin match v.Metrics_types.value with
  | Metrics_types.As_double x ->
    Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | Metrics_types.As_int x ->
    Pbrt.Encoder.key (6, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.int64_as_bits64 x encoder;
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exemplar x) encoder;
  ) v.Metrics_types.exemplars;
  Pbrt.Encoder.key (8, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Metrics_types.flags encoder;
  ()

let rec encode_gauge (v:Metrics_types.gauge) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_number_data_point x) encoder;
  ) v.Metrics_types.data_points;
  ()

let rec encode_aggregation_temporality (v:Metrics_types.aggregation_temporality) encoder =
  match v with
  | Metrics_types.Aggregation_temporality_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Metrics_types.Aggregation_temporality_delta -> Pbrt.Encoder.int_as_varint 1 encoder
  | Metrics_types.Aggregation_temporality_cumulative -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_sum (v:Metrics_types.sum) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_number_data_point x) encoder;
  ) v.Metrics_types.data_points;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_aggregation_temporality v.Metrics_types.aggregation_temporality encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.Metrics_types.is_monotonic encoder;
  ()

let rec encode_histogram_data_point (v:Metrics_types.histogram_data_point) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Metrics_types.attributes;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.start_time_unix_nano encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.time_unix_nano encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.count encoder;
  begin match v.Metrics_types.sum with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_bits64 x encoder;
    ) v.Metrics_types.bucket_counts;
  ) encoder;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Metrics_types.explicit_bounds;
  ) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exemplar x) encoder;
  ) v.Metrics_types.exemplars;
  Pbrt.Encoder.key (10, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Metrics_types.flags encoder;
  begin match v.Metrics_types.min with
  | Some x -> 
    Pbrt.Encoder.key (11, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  begin match v.Metrics_types.max with
  | Some x -> 
    Pbrt.Encoder.key (12, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  ()

let rec encode_histogram (v:Metrics_types.histogram) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_histogram_data_point x) encoder;
  ) v.Metrics_types.data_points;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_aggregation_temporality v.Metrics_types.aggregation_temporality encoder;
  ()

let rec encode_exponential_histogram_data_point_buckets (v:Metrics_types.exponential_histogram_data_point_buckets) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_zigzag v.Metrics_types.offset encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Metrics_types.bucket_counts;
  ) encoder;
  ()

let rec encode_exponential_histogram_data_point (v:Metrics_types.exponential_histogram_data_point) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Metrics_types.attributes;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.start_time_unix_nano encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.time_unix_nano encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.count encoder;
  begin match v.Metrics_types.sum with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_zigzag v.Metrics_types.scale encoder;
  Pbrt.Encoder.key (7, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.zero_count encoder;
  begin match v.Metrics_types.positive with
  | Some x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exponential_histogram_data_point_buckets x) encoder;
  | None -> ();
  end;
  begin match v.Metrics_types.negative with
  | Some x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exponential_histogram_data_point_buckets x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (10, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Metrics_types.flags encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exemplar x) encoder;
  ) v.Metrics_types.exemplars;
  begin match v.Metrics_types.min with
  | Some x -> 
    Pbrt.Encoder.key (12, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  begin match v.Metrics_types.max with
  | Some x -> 
    Pbrt.Encoder.key (13, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (14, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Metrics_types.zero_threshold encoder;
  ()

let rec encode_exponential_histogram (v:Metrics_types.exponential_histogram) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exponential_histogram_data_point x) encoder;
  ) v.Metrics_types.data_points;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_aggregation_temporality v.Metrics_types.aggregation_temporality encoder;
  ()

let rec encode_summary_data_point_value_at_quantile (v:Metrics_types.summary_data_point_value_at_quantile) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Metrics_types.quantile encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Metrics_types.value encoder;
  ()

let rec encode_summary_data_point (v:Metrics_types.summary_data_point) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Metrics_types.attributes;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.start_time_unix_nano encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.time_unix_nano encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Metrics_types.count encoder;
  Pbrt.Encoder.key (5, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Metrics_types.sum encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_data_point_value_at_quantile x) encoder;
  ) v.Metrics_types.quantile_values;
  Pbrt.Encoder.key (8, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Metrics_types.flags encoder;
  ()

let rec encode_summary (v:Metrics_types.summary) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_data_point x) encoder;
  ) v.Metrics_types.data_points;
  ()

let rec encode_metric_data (v:Metrics_types.metric_data) encoder = 
  begin match v with
  | Metrics_types.Gauge x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_gauge x) encoder;
  | Metrics_types.Sum x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_sum x) encoder;
  | Metrics_types.Histogram x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_histogram x) encoder;
  | Metrics_types.Exponential_histogram x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exponential_histogram x) encoder;
  | Metrics_types.Summary x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary x) encoder;
  end

and encode_metric (v:Metrics_types.metric) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_types.description encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_types.unit_ encoder;
  begin match v.Metrics_types.data with
  | Metrics_types.Gauge x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_gauge x) encoder;
  | Metrics_types.Sum x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_sum x) encoder;
  | Metrics_types.Histogram x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_histogram x) encoder;
  | Metrics_types.Exponential_histogram x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_exponential_histogram x) encoder;
  | Metrics_types.Summary x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary x) encoder;
  end;
  ()

let rec encode_scope_metrics (v:Metrics_types.scope_metrics) encoder = 
  begin match v.Metrics_types.scope with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_instrumentation_scope x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_metric x) encoder;
  ) v.Metrics_types.metrics;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_types.schema_url encoder;
  ()

let rec encode_resource_metrics (v:Metrics_types.resource_metrics) encoder = 
  begin match v.Metrics_types.resource with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Resource_pb.encode_resource x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_scope_metrics x) encoder;
  ) v.Metrics_types.scope_metrics;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_types.schema_url encoder;
  ()

let rec encode_metrics_data (v:Metrics_types.metrics_data) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_resource_metrics x) encoder;
  ) v.Metrics_types.resource_metrics;
  ()

let rec encode_data_point_flags (v:Metrics_types.data_point_flags) encoder =
  match v with
  | Metrics_types.Data_point_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Metrics_types.Data_point_flags_no_recorded_value_mask -> Pbrt.Encoder.int_as_varint 1 encoder
