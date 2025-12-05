[@@@ocaml.warning "-23-27-30-39-44"]

type exemplar_value =
  | As_double of float
  | As_int of int64

and exemplar = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable filtered_attributes : Common.key_value list;
  mutable time_unix_nano : int64;
  mutable value : exemplar_value option;
  mutable span_id : bytes;
  mutable trace_id : bytes;
}

type number_data_point_value =
  | As_double of float
  | As_int of int64

and number_data_point = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable value : number_data_point_value option;
  mutable exemplars : exemplar list;
  mutable flags : int32;
}

type gauge = {
  mutable data_points : number_data_point list;
}

type aggregation_temporality =
  | Aggregation_temporality_unspecified 
  | Aggregation_temporality_delta 
  | Aggregation_temporality_cumulative 

type sum = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable data_points : number_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
  mutable is_monotonic : bool;
}

type histogram_data_point = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 7 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable bucket_counts : int64 list;
  mutable explicit_bounds : float list;
  mutable exemplars : exemplar list;
  mutable flags : int32;
  mutable min : float;
  mutable max : float;
}

type histogram = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable data_points : histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

type exponential_histogram_data_point_buckets = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable offset : int32;
  mutable bucket_counts : int64 list;
}

type exponential_histogram_data_point = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 10 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable scale : int32;
  mutable zero_count : int64;
  mutable positive : exponential_histogram_data_point_buckets option;
  mutable negative : exponential_histogram_data_point_buckets option;
  mutable flags : int32;
  mutable exemplars : exemplar list;
  mutable min : float;
  mutable max : float;
  mutable zero_threshold : float;
}

type exponential_histogram = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable data_points : exponential_histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

type summary_data_point_value_at_quantile = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable quantile : float;
  mutable value : float;
}

type summary_data_point = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable quantile_values : summary_data_point_value_at_quantile list;
  mutable flags : int32;
}

type summary = {
  mutable data_points : summary_data_point list;
}

type metric_data =
  | Gauge of gauge
  | Sum of sum
  | Histogram of histogram
  | Exponential_histogram of exponential_histogram
  | Summary of summary

and metric = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable name : string;
  mutable description : string;
  mutable unit_ : string;
  mutable data : metric_data option;
  mutable metadata : Common.key_value list;
}

type scope_metrics = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable metrics : metric list;
  mutable schema_url : string;
}

type resource_metrics = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_metrics : scope_metrics list;
  mutable schema_url : string;
}

type metrics_data = {
  mutable resource_metrics : resource_metrics list;
}

type data_point_flags =
  | Data_point_flags_do_not_use 
  | Data_point_flags_no_recorded_value_mask 

let default_exemplar_value (): exemplar_value = As_double (0.)

let default_exemplar (): exemplar =
{
  _presence=Pbrt.Bitfield.empty;
  filtered_attributes=[];
  time_unix_nano=0L;
  value=None;
  span_id=Bytes.create 0;
  trace_id=Bytes.create 0;
}

let default_number_data_point_value (): number_data_point_value = As_double (0.)

let default_number_data_point (): number_data_point =
{
  _presence=Pbrt.Bitfield.empty;
  attributes=[];
  start_time_unix_nano=0L;
  time_unix_nano=0L;
  value=None;
  exemplars=[];
  flags=0l;
}

let default_gauge (): gauge =
{
  data_points=[];
}

let default_aggregation_temporality () = (Aggregation_temporality_unspecified:aggregation_temporality)

let default_sum (): sum =
{
  _presence=Pbrt.Bitfield.empty;
  data_points=[];
  aggregation_temporality=default_aggregation_temporality ();
  is_monotonic=false;
}

let default_histogram_data_point (): histogram_data_point =
{
  _presence=Pbrt.Bitfield.empty;
  attributes=[];
  start_time_unix_nano=0L;
  time_unix_nano=0L;
  count=0L;
  sum=0.;
  bucket_counts=[];
  explicit_bounds=[];
  exemplars=[];
  flags=0l;
  min=0.;
  max=0.;
}

let default_histogram (): histogram =
{
  _presence=Pbrt.Bitfield.empty;
  data_points=[];
  aggregation_temporality=default_aggregation_temporality ();
}

let default_exponential_histogram_data_point_buckets (): exponential_histogram_data_point_buckets =
{
  _presence=Pbrt.Bitfield.empty;
  offset=0l;
  bucket_counts=[];
}

let default_exponential_histogram_data_point (): exponential_histogram_data_point =
{
  _presence=Pbrt.Bitfield.empty;
  attributes=[];
  start_time_unix_nano=0L;
  time_unix_nano=0L;
  count=0L;
  sum=0.;
  scale=0l;
  zero_count=0L;
  positive=None;
  negative=None;
  flags=0l;
  exemplars=[];
  min=0.;
  max=0.;
  zero_threshold=0.;
}

let default_exponential_histogram (): exponential_histogram =
{
  _presence=Pbrt.Bitfield.empty;
  data_points=[];
  aggregation_temporality=default_aggregation_temporality ();
}

let default_summary_data_point_value_at_quantile (): summary_data_point_value_at_quantile =
{
  _presence=Pbrt.Bitfield.empty;
  quantile=0.;
  value=0.;
}

let default_summary_data_point (): summary_data_point =
{
  _presence=Pbrt.Bitfield.empty;
  attributes=[];
  start_time_unix_nano=0L;
  time_unix_nano=0L;
  count=0L;
  sum=0.;
  quantile_values=[];
  flags=0l;
}

let default_summary (): summary =
{
  data_points=[];
}

let default_metric_data (): metric_data = Gauge (default_gauge ())

let default_metric (): metric =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  description="";
  unit_="";
  data=None;
  metadata=[];
}

let default_scope_metrics (): scope_metrics =
{
  _presence=Pbrt.Bitfield.empty;
  scope=None;
  metrics=[];
  schema_url="";
}

let default_resource_metrics (): resource_metrics =
{
  _presence=Pbrt.Bitfield.empty;
  resource=None;
  scope_metrics=[];
  schema_url="";
}

let default_metrics_data (): metrics_data =
{
  resource_metrics=[];
}

let default_data_point_flags () = (Data_point_flags_do_not_use:data_point_flags)


(** {2 Make functions} *)

let[@inline] exemplar_has_time_unix_nano (self:exemplar) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] exemplar_has_span_id (self:exemplar) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] exemplar_has_trace_id (self:exemplar) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] exemplar_set_filtered_attributes (self:exemplar) (x:Common.key_value list) : unit =
  self.filtered_attributes <- x
let[@inline] exemplar_set_time_unix_nano (self:exemplar) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.time_unix_nano <- x
let[@inline] exemplar_set_value (self:exemplar) (x:exemplar_value) : unit =
  self.value <- Some x
let[@inline] exemplar_set_span_id (self:exemplar) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.span_id <- x
let[@inline] exemplar_set_trace_id (self:exemplar) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.trace_id <- x

let copy_exemplar (self:exemplar) : exemplar =
  { self with filtered_attributes = self.filtered_attributes }

let make_exemplar 
  ?(filtered_attributes=[])
  ?(time_unix_nano:int64 option)
  ?(value:exemplar_value option)
  ?(span_id:bytes option)
  ?(trace_id:bytes option)
  () : exemplar  =
  let _res = default_exemplar () in
  exemplar_set_filtered_attributes _res filtered_attributes;
  (match time_unix_nano with
  | None -> ()
  | Some v -> exemplar_set_time_unix_nano _res v);
  (match value with
  | None -> ()
  | Some v -> exemplar_set_value _res v);
  (match span_id with
  | None -> ()
  | Some v -> exemplar_set_span_id _res v);
  (match trace_id with
  | None -> ()
  | Some v -> exemplar_set_trace_id _res v);
  _res

let[@inline] number_data_point_has_start_time_unix_nano (self:number_data_point) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] number_data_point_has_time_unix_nano (self:number_data_point) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] number_data_point_has_flags (self:number_data_point) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] number_data_point_set_attributes (self:number_data_point) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] number_data_point_set_start_time_unix_nano (self:number_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start_time_unix_nano <- x
let[@inline] number_data_point_set_time_unix_nano (self:number_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.time_unix_nano <- x
let[@inline] number_data_point_set_value (self:number_data_point) (x:number_data_point_value) : unit =
  self.value <- Some x
let[@inline] number_data_point_set_exemplars (self:number_data_point) (x:exemplar list) : unit =
  self.exemplars <- x
let[@inline] number_data_point_set_flags (self:number_data_point) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.flags <- x

let copy_number_data_point (self:number_data_point) : number_data_point =
  { self with attributes = self.attributes }

let make_number_data_point 
  ?(attributes=[])
  ?(start_time_unix_nano:int64 option)
  ?(time_unix_nano:int64 option)
  ?(value:number_data_point_value option)
  ?(exemplars=[])
  ?(flags:int32 option)
  () : number_data_point  =
  let _res = default_number_data_point () in
  number_data_point_set_attributes _res attributes;
  (match start_time_unix_nano with
  | None -> ()
  | Some v -> number_data_point_set_start_time_unix_nano _res v);
  (match time_unix_nano with
  | None -> ()
  | Some v -> number_data_point_set_time_unix_nano _res v);
  (match value with
  | None -> ()
  | Some v -> number_data_point_set_value _res v);
  number_data_point_set_exemplars _res exemplars;
  (match flags with
  | None -> ()
  | Some v -> number_data_point_set_flags _res v);
  _res


let[@inline] gauge_set_data_points (self:gauge) (x:number_data_point list) : unit =
  self.data_points <- x

let copy_gauge (self:gauge) : gauge =
  { self with data_points = self.data_points }

let make_gauge 
  ?(data_points=[])
  () : gauge  =
  let _res = default_gauge () in
  gauge_set_data_points _res data_points;
  _res

let[@inline] sum_has_aggregation_temporality (self:sum) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] sum_has_is_monotonic (self:sum) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] sum_set_data_points (self:sum) (x:number_data_point list) : unit =
  self.data_points <- x
let[@inline] sum_set_aggregation_temporality (self:sum) (x:aggregation_temporality) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.aggregation_temporality <- x
let[@inline] sum_set_is_monotonic (self:sum) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.is_monotonic <- x

let copy_sum (self:sum) : sum =
  { self with data_points = self.data_points }

let make_sum 
  ?(data_points=[])
  ?(aggregation_temporality:aggregation_temporality option)
  ?(is_monotonic:bool option)
  () : sum  =
  let _res = default_sum () in
  sum_set_data_points _res data_points;
  (match aggregation_temporality with
  | None -> ()
  | Some v -> sum_set_aggregation_temporality _res v);
  (match is_monotonic with
  | None -> ()
  | Some v -> sum_set_is_monotonic _res v);
  _res

let[@inline] histogram_data_point_has_start_time_unix_nano (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] histogram_data_point_has_time_unix_nano (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] histogram_data_point_has_count (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] histogram_data_point_has_sum (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] histogram_data_point_has_flags (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] histogram_data_point_has_min (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] histogram_data_point_has_max (self:histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 6)

let[@inline] histogram_data_point_set_attributes (self:histogram_data_point) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] histogram_data_point_set_start_time_unix_nano (self:histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start_time_unix_nano <- x
let[@inline] histogram_data_point_set_time_unix_nano (self:histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.time_unix_nano <- x
let[@inline] histogram_data_point_set_count (self:histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.count <- x
let[@inline] histogram_data_point_set_sum (self:histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.sum <- x
let[@inline] histogram_data_point_set_bucket_counts (self:histogram_data_point) (x:int64 list) : unit =
  self.bucket_counts <- x
let[@inline] histogram_data_point_set_explicit_bounds (self:histogram_data_point) (x:float list) : unit =
  self.explicit_bounds <- x
let[@inline] histogram_data_point_set_exemplars (self:histogram_data_point) (x:exemplar list) : unit =
  self.exemplars <- x
let[@inline] histogram_data_point_set_flags (self:histogram_data_point) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.flags <- x
let[@inline] histogram_data_point_set_min (self:histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.min <- x
let[@inline] histogram_data_point_set_max (self:histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.max <- x

let copy_histogram_data_point (self:histogram_data_point) : histogram_data_point =
  { self with attributes = self.attributes }

let make_histogram_data_point 
  ?(attributes=[])
  ?(start_time_unix_nano:int64 option)
  ?(time_unix_nano:int64 option)
  ?(count:int64 option)
  ?(sum:float option)
  ?(bucket_counts=[])
  ?(explicit_bounds=[])
  ?(exemplars=[])
  ?(flags:int32 option)
  ?(min:float option)
  ?(max:float option)
  () : histogram_data_point  =
  let _res = default_histogram_data_point () in
  histogram_data_point_set_attributes _res attributes;
  (match start_time_unix_nano with
  | None -> ()
  | Some v -> histogram_data_point_set_start_time_unix_nano _res v);
  (match time_unix_nano with
  | None -> ()
  | Some v -> histogram_data_point_set_time_unix_nano _res v);
  (match count with
  | None -> ()
  | Some v -> histogram_data_point_set_count _res v);
  (match sum with
  | None -> ()
  | Some v -> histogram_data_point_set_sum _res v);
  histogram_data_point_set_bucket_counts _res bucket_counts;
  histogram_data_point_set_explicit_bounds _res explicit_bounds;
  histogram_data_point_set_exemplars _res exemplars;
  (match flags with
  | None -> ()
  | Some v -> histogram_data_point_set_flags _res v);
  (match min with
  | None -> ()
  | Some v -> histogram_data_point_set_min _res v);
  (match max with
  | None -> ()
  | Some v -> histogram_data_point_set_max _res v);
  _res

let[@inline] histogram_has_aggregation_temporality (self:histogram) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] histogram_set_data_points (self:histogram) (x:histogram_data_point list) : unit =
  self.data_points <- x
let[@inline] histogram_set_aggregation_temporality (self:histogram) (x:aggregation_temporality) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.aggregation_temporality <- x

let copy_histogram (self:histogram) : histogram =
  { self with data_points = self.data_points }

let make_histogram 
  ?(data_points=[])
  ?(aggregation_temporality:aggregation_temporality option)
  () : histogram  =
  let _res = default_histogram () in
  histogram_set_data_points _res data_points;
  (match aggregation_temporality with
  | None -> ()
  | Some v -> histogram_set_aggregation_temporality _res v);
  _res

let[@inline] exponential_histogram_data_point_buckets_has_offset (self:exponential_histogram_data_point_buckets) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] exponential_histogram_data_point_buckets_set_offset (self:exponential_histogram_data_point_buckets) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.offset <- x
let[@inline] exponential_histogram_data_point_buckets_set_bucket_counts (self:exponential_histogram_data_point_buckets) (x:int64 list) : unit =
  self.bucket_counts <- x

let copy_exponential_histogram_data_point_buckets (self:exponential_histogram_data_point_buckets) : exponential_histogram_data_point_buckets =
  { self with offset = self.offset }

let make_exponential_histogram_data_point_buckets 
  ?(offset:int32 option)
  ?(bucket_counts=[])
  () : exponential_histogram_data_point_buckets  =
  let _res = default_exponential_histogram_data_point_buckets () in
  (match offset with
  | None -> ()
  | Some v -> exponential_histogram_data_point_buckets_set_offset _res v);
  exponential_histogram_data_point_buckets_set_bucket_counts _res bucket_counts;
  _res

let[@inline] exponential_histogram_data_point_has_start_time_unix_nano (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] exponential_histogram_data_point_has_time_unix_nano (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] exponential_histogram_data_point_has_count (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] exponential_histogram_data_point_has_sum (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] exponential_histogram_data_point_has_scale (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] exponential_histogram_data_point_has_zero_count (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] exponential_histogram_data_point_has_flags (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] exponential_histogram_data_point_has_min (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] exponential_histogram_data_point_has_max (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 8)
let[@inline] exponential_histogram_data_point_has_zero_threshold (self:exponential_histogram_data_point) : bool = (Pbrt.Bitfield.get self._presence 9)

let[@inline] exponential_histogram_data_point_set_attributes (self:exponential_histogram_data_point) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] exponential_histogram_data_point_set_start_time_unix_nano (self:exponential_histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start_time_unix_nano <- x
let[@inline] exponential_histogram_data_point_set_time_unix_nano (self:exponential_histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.time_unix_nano <- x
let[@inline] exponential_histogram_data_point_set_count (self:exponential_histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.count <- x
let[@inline] exponential_histogram_data_point_set_sum (self:exponential_histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.sum <- x
let[@inline] exponential_histogram_data_point_set_scale (self:exponential_histogram_data_point) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.scale <- x
let[@inline] exponential_histogram_data_point_set_zero_count (self:exponential_histogram_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.zero_count <- x
let[@inline] exponential_histogram_data_point_set_positive (self:exponential_histogram_data_point) (x:exponential_histogram_data_point_buckets) : unit =
  self.positive <- Some x
let[@inline] exponential_histogram_data_point_set_negative (self:exponential_histogram_data_point) (x:exponential_histogram_data_point_buckets) : unit =
  self.negative <- Some x
let[@inline] exponential_histogram_data_point_set_flags (self:exponential_histogram_data_point) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.flags <- x
let[@inline] exponential_histogram_data_point_set_exemplars (self:exponential_histogram_data_point) (x:exemplar list) : unit =
  self.exemplars <- x
let[@inline] exponential_histogram_data_point_set_min (self:exponential_histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.min <- x
let[@inline] exponential_histogram_data_point_set_max (self:exponential_histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.max <- x
let[@inline] exponential_histogram_data_point_set_zero_threshold (self:exponential_histogram_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 9); self.zero_threshold <- x

let copy_exponential_histogram_data_point (self:exponential_histogram_data_point) : exponential_histogram_data_point =
  { self with attributes = self.attributes }

let make_exponential_histogram_data_point 
  ?(attributes=[])
  ?(start_time_unix_nano:int64 option)
  ?(time_unix_nano:int64 option)
  ?(count:int64 option)
  ?(sum:float option)
  ?(scale:int32 option)
  ?(zero_count:int64 option)
  ?(positive:exponential_histogram_data_point_buckets option)
  ?(negative:exponential_histogram_data_point_buckets option)
  ?(flags:int32 option)
  ?(exemplars=[])
  ?(min:float option)
  ?(max:float option)
  ?(zero_threshold:float option)
  () : exponential_histogram_data_point  =
  let _res = default_exponential_histogram_data_point () in
  exponential_histogram_data_point_set_attributes _res attributes;
  (match start_time_unix_nano with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_start_time_unix_nano _res v);
  (match time_unix_nano with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_time_unix_nano _res v);
  (match count with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_count _res v);
  (match sum with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_sum _res v);
  (match scale with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_scale _res v);
  (match zero_count with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_zero_count _res v);
  (match positive with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_positive _res v);
  (match negative with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_negative _res v);
  (match flags with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_flags _res v);
  exponential_histogram_data_point_set_exemplars _res exemplars;
  (match min with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_min _res v);
  (match max with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_max _res v);
  (match zero_threshold with
  | None -> ()
  | Some v -> exponential_histogram_data_point_set_zero_threshold _res v);
  _res

let[@inline] exponential_histogram_has_aggregation_temporality (self:exponential_histogram) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] exponential_histogram_set_data_points (self:exponential_histogram) (x:exponential_histogram_data_point list) : unit =
  self.data_points <- x
let[@inline] exponential_histogram_set_aggregation_temporality (self:exponential_histogram) (x:aggregation_temporality) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.aggregation_temporality <- x

let copy_exponential_histogram (self:exponential_histogram) : exponential_histogram =
  { self with data_points = self.data_points }

let make_exponential_histogram 
  ?(data_points=[])
  ?(aggregation_temporality:aggregation_temporality option)
  () : exponential_histogram  =
  let _res = default_exponential_histogram () in
  exponential_histogram_set_data_points _res data_points;
  (match aggregation_temporality with
  | None -> ()
  | Some v -> exponential_histogram_set_aggregation_temporality _res v);
  _res

let[@inline] summary_data_point_value_at_quantile_has_quantile (self:summary_data_point_value_at_quantile) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] summary_data_point_value_at_quantile_has_value (self:summary_data_point_value_at_quantile) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] summary_data_point_value_at_quantile_set_quantile (self:summary_data_point_value_at_quantile) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.quantile <- x
let[@inline] summary_data_point_value_at_quantile_set_value (self:summary_data_point_value_at_quantile) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.value <- x

let copy_summary_data_point_value_at_quantile (self:summary_data_point_value_at_quantile) : summary_data_point_value_at_quantile =
  { self with quantile = self.quantile }

let make_summary_data_point_value_at_quantile 
  ?(quantile:float option)
  ?(value:float option)
  () : summary_data_point_value_at_quantile  =
  let _res = default_summary_data_point_value_at_quantile () in
  (match quantile with
  | None -> ()
  | Some v -> summary_data_point_value_at_quantile_set_quantile _res v);
  (match value with
  | None -> ()
  | Some v -> summary_data_point_value_at_quantile_set_value _res v);
  _res

let[@inline] summary_data_point_has_start_time_unix_nano (self:summary_data_point) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] summary_data_point_has_time_unix_nano (self:summary_data_point) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] summary_data_point_has_count (self:summary_data_point) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] summary_data_point_has_sum (self:summary_data_point) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] summary_data_point_has_flags (self:summary_data_point) : bool = (Pbrt.Bitfield.get self._presence 4)

let[@inline] summary_data_point_set_attributes (self:summary_data_point) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] summary_data_point_set_start_time_unix_nano (self:summary_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start_time_unix_nano <- x
let[@inline] summary_data_point_set_time_unix_nano (self:summary_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.time_unix_nano <- x
let[@inline] summary_data_point_set_count (self:summary_data_point) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.count <- x
let[@inline] summary_data_point_set_sum (self:summary_data_point) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.sum <- x
let[@inline] summary_data_point_set_quantile_values (self:summary_data_point) (x:summary_data_point_value_at_quantile list) : unit =
  self.quantile_values <- x
let[@inline] summary_data_point_set_flags (self:summary_data_point) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.flags <- x

let copy_summary_data_point (self:summary_data_point) : summary_data_point =
  { self with attributes = self.attributes }

let make_summary_data_point 
  ?(attributes=[])
  ?(start_time_unix_nano:int64 option)
  ?(time_unix_nano:int64 option)
  ?(count:int64 option)
  ?(sum:float option)
  ?(quantile_values=[])
  ?(flags:int32 option)
  () : summary_data_point  =
  let _res = default_summary_data_point () in
  summary_data_point_set_attributes _res attributes;
  (match start_time_unix_nano with
  | None -> ()
  | Some v -> summary_data_point_set_start_time_unix_nano _res v);
  (match time_unix_nano with
  | None -> ()
  | Some v -> summary_data_point_set_time_unix_nano _res v);
  (match count with
  | None -> ()
  | Some v -> summary_data_point_set_count _res v);
  (match sum with
  | None -> ()
  | Some v -> summary_data_point_set_sum _res v);
  summary_data_point_set_quantile_values _res quantile_values;
  (match flags with
  | None -> ()
  | Some v -> summary_data_point_set_flags _res v);
  _res


let[@inline] summary_set_data_points (self:summary) (x:summary_data_point list) : unit =
  self.data_points <- x

let copy_summary (self:summary) : summary =
  { self with data_points = self.data_points }

let make_summary 
  ?(data_points=[])
  () : summary  =
  let _res = default_summary () in
  summary_set_data_points _res data_points;
  _res

let[@inline] metric_has_name (self:metric) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] metric_has_description (self:metric) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] metric_has_unit_ (self:metric) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] metric_set_name (self:metric) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] metric_set_description (self:metric) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.description <- x
let[@inline] metric_set_unit_ (self:metric) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.unit_ <- x
let[@inline] metric_set_data (self:metric) (x:metric_data) : unit =
  self.data <- Some x
let[@inline] metric_set_metadata (self:metric) (x:Common.key_value list) : unit =
  self.metadata <- x

let copy_metric (self:metric) : metric =
  { self with name = self.name }

let make_metric 
  ?(name:string option)
  ?(description:string option)
  ?(unit_:string option)
  ?(data:metric_data option)
  ?(metadata=[])
  () : metric  =
  let _res = default_metric () in
  (match name with
  | None -> ()
  | Some v -> metric_set_name _res v);
  (match description with
  | None -> ()
  | Some v -> metric_set_description _res v);
  (match unit_ with
  | None -> ()
  | Some v -> metric_set_unit_ _res v);
  (match data with
  | None -> ()
  | Some v -> metric_set_data _res v);
  metric_set_metadata _res metadata;
  _res

let[@inline] scope_metrics_has_schema_url (self:scope_metrics) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] scope_metrics_set_scope (self:scope_metrics) (x:Common.instrumentation_scope) : unit =
  self.scope <- Some x
let[@inline] scope_metrics_set_metrics (self:scope_metrics) (x:metric list) : unit =
  self.metrics <- x
let[@inline] scope_metrics_set_schema_url (self:scope_metrics) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_scope_metrics (self:scope_metrics) : scope_metrics =
  { self with scope = self.scope }

let make_scope_metrics 
  ?(scope:Common.instrumentation_scope option)
  ?(metrics=[])
  ?(schema_url:string option)
  () : scope_metrics  =
  let _res = default_scope_metrics () in
  (match scope with
  | None -> ()
  | Some v -> scope_metrics_set_scope _res v);
  scope_metrics_set_metrics _res metrics;
  (match schema_url with
  | None -> ()
  | Some v -> scope_metrics_set_schema_url _res v);
  _res

let[@inline] resource_metrics_has_schema_url (self:resource_metrics) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] resource_metrics_set_resource (self:resource_metrics) (x:Resource.resource) : unit =
  self.resource <- Some x
let[@inline] resource_metrics_set_scope_metrics (self:resource_metrics) (x:scope_metrics list) : unit =
  self.scope_metrics <- x
let[@inline] resource_metrics_set_schema_url (self:resource_metrics) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_resource_metrics (self:resource_metrics) : resource_metrics =
  { self with resource = self.resource }

let make_resource_metrics 
  ?(resource:Resource.resource option)
  ?(scope_metrics=[])
  ?(schema_url:string option)
  () : resource_metrics  =
  let _res = default_resource_metrics () in
  (match resource with
  | None -> ()
  | Some v -> resource_metrics_set_resource _res v);
  resource_metrics_set_scope_metrics _res scope_metrics;
  (match schema_url with
  | None -> ()
  | Some v -> resource_metrics_set_schema_url _res v);
  _res


let[@inline] metrics_data_set_resource_metrics (self:metrics_data) (x:resource_metrics list) : unit =
  self.resource_metrics <- x

let copy_metrics_data (self:metrics_data) : metrics_data =
  { self with resource_metrics = self.resource_metrics }

let make_metrics_data 
  ?(resource_metrics=[])
  () : metrics_data  =
  let _res = default_metrics_data () in
  metrics_data_set_resource_metrics _res resource_metrics;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_exemplar_value fmt (v:exemplar_value) =
  match v with
  | As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_exemplar fmt (v:exemplar) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "filtered_attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.filtered_attributes;
    Pbrt.Pp.pp_record_field ~absent:(not (exemplar_has_time_unix_nano v)) ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" (Pbrt.Pp.pp_option pp_exemplar_value) fmt v.value;
    Pbrt.Pp.pp_record_field ~absent:(not (exemplar_has_span_id v)) ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    Pbrt.Pp.pp_record_field ~absent:(not (exemplar_has_trace_id v)) ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_number_data_point_value fmt (v:number_data_point_value) =
  match v with
  | As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_number_data_point fmt (v:number_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~absent:(not (number_data_point_has_start_time_unix_nano v)) ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (number_data_point_has_time_unix_nano v)) ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" (Pbrt.Pp.pp_option pp_number_data_point_value) fmt v.value;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~absent:(not (number_data_point_has_flags v)) ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_gauge fmt (v:gauge) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_aggregation_temporality fmt (v:aggregation_temporality) =
  match v with
  | Aggregation_temporality_unspecified -> Format.fprintf fmt "Aggregation_temporality_unspecified"
  | Aggregation_temporality_delta -> Format.fprintf fmt "Aggregation_temporality_delta"
  | Aggregation_temporality_cumulative -> Format.fprintf fmt "Aggregation_temporality_cumulative"

let rec pp_sum fmt (v:sum) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~absent:(not (sum_has_aggregation_temporality v)) ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
    Pbrt.Pp.pp_record_field ~absent:(not (sum_has_is_monotonic v)) ~first:false "is_monotonic" Pbrt.Pp.pp_bool fmt v.is_monotonic;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram_data_point fmt (v:histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_start_time_unix_nano v)) ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_time_unix_nano v)) ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_count v)) ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_sum v)) ~first:false "sum" Pbrt.Pp.pp_float fmt v.sum;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.bucket_counts;
    Pbrt.Pp.pp_record_field ~first:false "explicit_bounds" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.explicit_bounds;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_flags v)) ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_min v)) ~first:false "min" Pbrt.Pp.pp_float fmt v.min;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_data_point_has_max v)) ~first:false "max" Pbrt.Pp.pp_float fmt v.max;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram fmt (v:histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_histogram_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~absent:(not (histogram_has_aggregation_temporality v)) ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point_buckets fmt (v:exponential_histogram_data_point_buckets) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_buckets_has_offset v)) ~first:true "offset" Pbrt.Pp.pp_int32 fmt v.offset;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.bucket_counts;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point fmt (v:exponential_histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_start_time_unix_nano v)) ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_time_unix_nano v)) ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_count v)) ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_sum v)) ~first:false "sum" Pbrt.Pp.pp_float fmt v.sum;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_scale v)) ~first:false "scale" Pbrt.Pp.pp_int32 fmt v.scale;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_zero_count v)) ~first:false "zero_count" Pbrt.Pp.pp_int64 fmt v.zero_count;
    Pbrt.Pp.pp_record_field ~first:false "positive" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.positive;
    Pbrt.Pp.pp_record_field ~first:false "negative" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.negative;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_flags v)) ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_min v)) ~first:false "min" Pbrt.Pp.pp_float fmt v.min;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_max v)) ~first:false "max" Pbrt.Pp.pp_float fmt v.max;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_data_point_has_zero_threshold v)) ~first:false "zero_threshold" Pbrt.Pp.pp_float fmt v.zero_threshold;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram fmt (v:exponential_histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_exponential_histogram_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~absent:(not (exponential_histogram_has_aggregation_temporality v)) ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point_value_at_quantile fmt (v:summary_data_point_value_at_quantile) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_value_at_quantile_has_quantile v)) ~first:true "quantile" Pbrt.Pp.pp_float fmt v.quantile;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_value_at_quantile_has_value v)) ~first:false "value" Pbrt.Pp.pp_float fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point fmt (v:summary_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_has_start_time_unix_nano v)) ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_has_time_unix_nano v)) ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_has_count v)) ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_has_sum v)) ~first:false "sum" Pbrt.Pp.pp_float fmt v.sum;
    Pbrt.Pp.pp_record_field ~first:false "quantile_values" (Pbrt.Pp.pp_list pp_summary_data_point_value_at_quantile) fmt v.quantile_values;
    Pbrt.Pp.pp_record_field ~absent:(not (summary_data_point_has_flags v)) ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary fmt (v:summary) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_summary_data_point) fmt v.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metric_data fmt (v:metric_data) =
  match v with
  | Gauge x -> Format.fprintf fmt "@[<hv2>Gauge(@,%a)@]" pp_gauge x
  | Sum x -> Format.fprintf fmt "@[<hv2>Sum(@,%a)@]" pp_sum x
  | Histogram x -> Format.fprintf fmt "@[<hv2>Histogram(@,%a)@]" pp_histogram x
  | Exponential_histogram x -> Format.fprintf fmt "@[<hv2>Exponential_histogram(@,%a)@]" pp_exponential_histogram x
  | Summary x -> Format.fprintf fmt "@[<hv2>Summary(@,%a)@]" pp_summary x

and pp_metric fmt (v:metric) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (metric_has_name v)) ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~absent:(not (metric_has_description v)) ~first:false "description" Pbrt.Pp.pp_string fmt v.description;
    Pbrt.Pp.pp_record_field ~absent:(not (metric_has_unit_ v)) ~first:false "unit_" Pbrt.Pp.pp_string fmt v.unit_;
    Pbrt.Pp.pp_record_field ~first:false "data" (Pbrt.Pp.pp_option pp_metric_data) fmt v.data;
    Pbrt.Pp.pp_record_field ~first:false "metadata" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.metadata;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_metrics fmt (v:scope_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "metrics" (Pbrt.Pp.pp_list pp_metric) fmt v.metrics;
    Pbrt.Pp.pp_record_field ~absent:(not (scope_metrics_has_schema_url v)) ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_metrics fmt (v:resource_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_metrics" (Pbrt.Pp.pp_list pp_scope_metrics) fmt v.scope_metrics;
    Pbrt.Pp.pp_record_field ~absent:(not (resource_metrics_has_schema_url v)) ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metrics_data fmt (v:metrics_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list pp_resource_metrics) fmt v.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_data_point_flags fmt (v:data_point_flags) =
  match v with
  | Data_point_flags_do_not_use -> Format.fprintf fmt "Data_point_flags_do_not_use"
  | Data_point_flags_no_recorded_value_mask -> Format.fprintf fmt "Data_point_flags_no_recorded_value_mask"

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_exemplar_value (v:exemplar_value) encoder = 
  begin match v with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end

and encode_pb_exemplar (v:exemplar) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.filtered_attributes encoder;
  if exemplar_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  begin match v.value with
  | None -> ()
  | Some (As_double x) ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  | Some (As_int x) ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end;
  if exemplar_has_span_id v then (
    Pbrt.Encoder.bytes v.span_id encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  );
  if exemplar_has_trace_id v then (
    Pbrt.Encoder.bytes v.trace_id encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_number_data_point_value (v:number_data_point_value) encoder = 
  begin match v with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end

and encode_pb_number_data_point (v:number_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if number_data_point_has_start_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  if number_data_point_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  );
  begin match v.value with
  | None -> ()
  | Some (As_double x) ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | Some (As_int x) ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  if number_data_point_has_flags v then (
    Pbrt.Encoder.int32_as_varint v.flags encoder;
    Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_gauge (v:gauge) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_number_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  ()

let rec encode_pb_aggregation_temporality (v:aggregation_temporality) encoder =
  match v with
  | Aggregation_temporality_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Aggregation_temporality_delta -> Pbrt.Encoder.int_as_varint 1 encoder
  | Aggregation_temporality_cumulative -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_sum (v:sum) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_number_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  if sum_has_aggregation_temporality v then (
    encode_pb_aggregation_temporality v.aggregation_temporality encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if sum_has_is_monotonic v then (
    Pbrt.Encoder.bool v.is_monotonic encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_histogram_data_point (v:histogram_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if histogram_data_point_has_start_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  if histogram_data_point_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  );
  if histogram_data_point_has_count v then (
    Pbrt.Encoder.int64_as_bits64 v.count encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  );
  if histogram_data_point_has_sum v then (
    Pbrt.Encoder.float_as_bits64 v.sum encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  );
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.int64_as_bits64 x encoder;
    ) lst encoder;
  ) v.bucket_counts encoder;
  Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) lst encoder;
  ) v.explicit_bounds encoder;
  Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  if histogram_data_point_has_flags v then (
    Pbrt.Encoder.int32_as_varint v.flags encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  );
  if histogram_data_point_has_min v then (
    Pbrt.Encoder.float_as_bits64 v.min encoder;
    Pbrt.Encoder.key 11 Pbrt.Bits64 encoder; 
  );
  if histogram_data_point_has_max v then (
    Pbrt.Encoder.float_as_bits64 v.max encoder;
    Pbrt.Encoder.key 12 Pbrt.Bits64 encoder; 
  );
  ()

let rec encode_pb_histogram (v:histogram) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_histogram_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  if histogram_has_aggregation_temporality v then (
    encode_pb_aggregation_temporality v.aggregation_temporality encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_exponential_histogram_data_point_buckets (v:exponential_histogram_data_point_buckets) encoder = 
  if exponential_histogram_data_point_buckets_has_offset v then (
    Pbrt.Encoder.int32_as_zigzag v.offset encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.int64_as_varint x encoder;
    ) lst encoder;
  ) v.bucket_counts encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_exponential_histogram_data_point (v:exponential_histogram_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if exponential_histogram_data_point_has_start_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_count v then (
    Pbrt.Encoder.int64_as_bits64 v.count encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_sum v then (
    Pbrt.Encoder.float_as_bits64 v.sum encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_scale v then (
    Pbrt.Encoder.int32_as_zigzag v.scale encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  );
  if exponential_histogram_data_point_has_zero_count v then (
    Pbrt.Encoder.int64_as_bits64 v.zero_count encoder;
    Pbrt.Encoder.key 7 Pbrt.Bits64 encoder; 
  );
  begin match v.positive with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point_buckets x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  begin match v.negative with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point_buckets x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  if exponential_histogram_data_point_has_flags v then (
    Pbrt.Encoder.int32_as_varint v.flags encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  if exponential_histogram_data_point_has_min v then (
    Pbrt.Encoder.float_as_bits64 v.min encoder;
    Pbrt.Encoder.key 12 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_max v then (
    Pbrt.Encoder.float_as_bits64 v.max encoder;
    Pbrt.Encoder.key 13 Pbrt.Bits64 encoder; 
  );
  if exponential_histogram_data_point_has_zero_threshold v then (
    Pbrt.Encoder.float_as_bits64 v.zero_threshold encoder;
    Pbrt.Encoder.key 14 Pbrt.Bits64 encoder; 
  );
  ()

let rec encode_pb_exponential_histogram (v:exponential_histogram) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  if exponential_histogram_has_aggregation_temporality v then (
    encode_pb_aggregation_temporality v.aggregation_temporality encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_summary_data_point_value_at_quantile (v:summary_data_point_value_at_quantile) encoder = 
  if summary_data_point_value_at_quantile_has_quantile v then (
    Pbrt.Encoder.float_as_bits64 v.quantile encoder;
    Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  );
  if summary_data_point_value_at_quantile_has_value v then (
    Pbrt.Encoder.float_as_bits64 v.value encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  ()

let rec encode_pb_summary_data_point (v:summary_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if summary_data_point_has_start_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  if summary_data_point_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  );
  if summary_data_point_has_count v then (
    Pbrt.Encoder.int64_as_bits64 v.count encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  );
  if summary_data_point_has_sum v then (
    Pbrt.Encoder.float_as_bits64 v.sum encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_summary_data_point_value_at_quantile x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.quantile_values encoder;
  if summary_data_point_has_flags v then (
    Pbrt.Encoder.int32_as_varint v.flags encoder;
    Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_summary (v:summary) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_summary_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  ()

let rec encode_pb_metric_data (v:metric_data) encoder = 
  begin match v with
  | Gauge x ->
    Pbrt.Encoder.nested encode_pb_gauge x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Sum x ->
    Pbrt.Encoder.nested encode_pb_sum x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  | Histogram x ->
    Pbrt.Encoder.nested encode_pb_histogram x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | Exponential_histogram x ->
    Pbrt.Encoder.nested encode_pb_exponential_histogram x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  | Summary x ->
    Pbrt.Encoder.nested encode_pb_summary x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  end

and encode_pb_metric (v:metric) encoder = 
  if metric_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if metric_has_description v then (
    Pbrt.Encoder.string v.description encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if metric_has_unit_ v then (
    Pbrt.Encoder.string v.unit_ encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  begin match v.data with
  | None -> ()
  | Some (Gauge x) ->
    Pbrt.Encoder.nested encode_pb_gauge x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Some (Sum x) ->
    Pbrt.Encoder.nested encode_pb_sum x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  | Some (Histogram x) ->
    Pbrt.Encoder.nested encode_pb_histogram x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | Some (Exponential_histogram x) ->
    Pbrt.Encoder.nested encode_pb_exponential_histogram x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  | Some (Summary x) ->
    Pbrt.Encoder.nested encode_pb_summary x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 12 Pbrt.Bytes encoder; 
  ) v.metadata encoder;
  ()

let rec encode_pb_scope_metrics (v:scope_metrics) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_metric x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.metrics encoder;
  if scope_metrics_has_schema_url v then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_resource_metrics (v:resource_metrics) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_scope_metrics x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_metrics encoder;
  if resource_metrics_has_schema_url v then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_metrics_data (v:metrics_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_resource_metrics x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_metrics encoder;
  ()

let rec encode_pb_data_point_flags (v:data_point_flags) encoder =
  match v with
  | Data_point_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Data_point_flags_no_recorded_value_mask -> Pbrt.Encoder.int_as_varint 1 encoder

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_exemplar_value d = 
  let rec loop () = 
    let ret:exemplar_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "exemplar_value"
      | Some (3, _) -> (As_double (Pbrt.Decoder.float_as_bits64 d) : exemplar_value) 
      | Some (6, _) -> (As_int (Pbrt.Decoder.int64_as_bits64 d) : exemplar_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_exemplar d =
  let v = default_exemplar () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      exemplar_set_filtered_attributes v (List.rev v.filtered_attributes);
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      exemplar_set_filtered_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.filtered_attributes);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 7 pk
    | Some (2, Pbrt.Bits64) -> begin
      exemplar_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      exemplar_set_value v (As_double (Pbrt.Decoder.float_as_bits64 d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 3 pk
    | Some (6, Pbrt.Bits64) -> begin
      exemplar_set_value v (As_int (Pbrt.Decoder.int64_as_bits64 d));
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 6 pk
    | Some (4, Pbrt.Bytes) -> begin
      exemplar_set_span_id v (Pbrt.Decoder.bytes d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      exemplar_set_trace_id v (Pbrt.Decoder.bytes d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exemplar" 5 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : exemplar)

let rec decode_pb_number_data_point_value d = 
  let rec loop () = 
    let ret:number_data_point_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "number_data_point_value"
      | Some (4, _) -> (As_double (Pbrt.Decoder.float_as_bits64 d) : number_data_point_value) 
      | Some (6, _) -> (As_int (Pbrt.Decoder.int64_as_bits64 d) : number_data_point_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_number_data_point d =
  let v = default_number_data_point () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      number_data_point_set_exemplars v (List.rev v.exemplars);
      number_data_point_set_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      number_data_point_set_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 7 pk
    | Some (2, Pbrt.Bits64) -> begin
      number_data_point_set_start_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      number_data_point_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 3 pk
    | Some (4, Pbrt.Bits64) -> begin
      number_data_point_set_value v (As_double (Pbrt.Decoder.float_as_bits64 d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 4 pk
    | Some (6, Pbrt.Bits64) -> begin
      number_data_point_set_value v (As_int (Pbrt.Decoder.int64_as_bits64 d));
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 6 pk
    | Some (5, Pbrt.Bytes) -> begin
      number_data_point_set_exemplars v ((decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 5 pk
    | Some (8, Pbrt.Varint) -> begin
      number_data_point_set_flags v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "number_data_point" 8 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : number_data_point)

let rec decode_pb_gauge d =
  let v = default_gauge () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      gauge_set_data_points v (List.rev v.data_points);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      gauge_set_data_points v ((decode_pb_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "gauge" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : gauge)

let rec decode_pb_aggregation_temporality d : aggregation_temporality = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> Aggregation_temporality_unspecified
  | 1 -> Aggregation_temporality_delta
  | 2 -> Aggregation_temporality_cumulative
  | _ -> Pbrt.Decoder.malformed_variant "aggregation_temporality"

let rec decode_pb_sum d =
  let v = default_sum () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      sum_set_data_points v (List.rev v.data_points);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      sum_set_data_points v ((decode_pb_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "sum" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      sum_set_aggregation_temporality v (decode_pb_aggregation_temporality d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "sum" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      sum_set_is_monotonic v (Pbrt.Decoder.bool d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "sum" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : sum)

let rec decode_pb_histogram_data_point d =
  let v = default_histogram_data_point () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      histogram_data_point_set_exemplars v (List.rev v.exemplars);
      histogram_data_point_set_explicit_bounds v (List.rev v.explicit_bounds);
      histogram_data_point_set_bucket_counts v (List.rev v.bucket_counts);
      histogram_data_point_set_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (9, Pbrt.Bytes) -> begin
      histogram_data_point_set_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 9 pk
    | Some (2, Pbrt.Bits64) -> begin
      histogram_data_point_set_start_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      histogram_data_point_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 3 pk
    | Some (4, Pbrt.Bits64) -> begin
      histogram_data_point_set_count v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 4 pk
    | Some (5, Pbrt.Bits64) -> begin
      histogram_data_point_set_sum v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 5 pk
    | Some (6, Pbrt.Bytes) -> begin
      histogram_data_point_set_bucket_counts v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_bits64 d)::l) [] d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 6 pk
    | Some (7, Pbrt.Bytes) -> begin
      histogram_data_point_set_explicit_bounds v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 7 pk
    | Some (8, Pbrt.Bytes) -> begin
      histogram_data_point_set_exemplars v ((decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 8 pk
    | Some (10, Pbrt.Varint) -> begin
      histogram_data_point_set_flags v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 10 pk
    | Some (11, Pbrt.Bits64) -> begin
      histogram_data_point_set_min v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 11 pk
    | Some (12, Pbrt.Bits64) -> begin
      histogram_data_point_set_max v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram_data_point" 12 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : histogram_data_point)

let rec decode_pb_histogram d =
  let v = default_histogram () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      histogram_set_data_points v (List.rev v.data_points);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      histogram_set_data_points v ((decode_pb_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      histogram_set_aggregation_temporality v (decode_pb_aggregation_temporality d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "histogram" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : histogram)

let rec decode_pb_exponential_histogram_data_point_buckets d =
  let v = default_exponential_histogram_data_point_buckets () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      exponential_histogram_data_point_buckets_set_bucket_counts v (List.rev v.bucket_counts);
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      exponential_histogram_data_point_buckets_set_offset v (Pbrt.Decoder.int32_as_zigzag d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point_buckets" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      exponential_histogram_data_point_buckets_set_bucket_counts v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point_buckets" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : exponential_histogram_data_point_buckets)

let rec decode_pb_exponential_histogram_data_point d =
  let v = default_exponential_histogram_data_point () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      exponential_histogram_data_point_set_exemplars v (List.rev v.exemplars);
      exponential_histogram_data_point_set_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      exponential_histogram_data_point_set_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 1 pk
    | Some (2, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_start_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 3 pk
    | Some (4, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_count v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 4 pk
    | Some (5, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_sum v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 5 pk
    | Some (6, Pbrt.Varint) -> begin
      exponential_histogram_data_point_set_scale v (Pbrt.Decoder.int32_as_zigzag d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 6 pk
    | Some (7, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_zero_count v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 7 pk
    | Some (8, Pbrt.Bytes) -> begin
      exponential_histogram_data_point_set_positive v (decode_pb_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 8 pk
    | Some (9, Pbrt.Bytes) -> begin
      exponential_histogram_data_point_set_negative v (decode_pb_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 9 pk
    | Some (10, Pbrt.Varint) -> begin
      exponential_histogram_data_point_set_flags v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 10 pk
    | Some (11, Pbrt.Bytes) -> begin
      exponential_histogram_data_point_set_exemplars v ((decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 11 pk
    | Some (12, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_min v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 12 pk
    | Some (13, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_max v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 13 pk
    | Some (14, Pbrt.Bits64) -> begin
      exponential_histogram_data_point_set_zero_threshold v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram_data_point" 14 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : exponential_histogram_data_point)

let rec decode_pb_exponential_histogram d =
  let v = default_exponential_histogram () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      exponential_histogram_set_data_points v (List.rev v.data_points);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      exponential_histogram_set_data_points v ((decode_pb_exponential_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      exponential_histogram_set_aggregation_temporality v (decode_pb_aggregation_temporality d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "exponential_histogram" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : exponential_histogram)

let rec decode_pb_summary_data_point_value_at_quantile d =
  let v = default_summary_data_point_value_at_quantile () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      summary_data_point_value_at_quantile_set_quantile v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point_value_at_quantile" 1 pk
    | Some (2, Pbrt.Bits64) -> begin
      summary_data_point_value_at_quantile_set_value v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point_value_at_quantile" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : summary_data_point_value_at_quantile)

let rec decode_pb_summary_data_point d =
  let v = default_summary_data_point () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      summary_data_point_set_quantile_values v (List.rev v.quantile_values);
      summary_data_point_set_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      summary_data_point_set_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 7 pk
    | Some (2, Pbrt.Bits64) -> begin
      summary_data_point_set_start_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      summary_data_point_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 3 pk
    | Some (4, Pbrt.Bits64) -> begin
      summary_data_point_set_count v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 4 pk
    | Some (5, Pbrt.Bits64) -> begin
      summary_data_point_set_sum v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 5 pk
    | Some (6, Pbrt.Bytes) -> begin
      summary_data_point_set_quantile_values v ((decode_pb_summary_data_point_value_at_quantile (Pbrt.Decoder.nested d)) :: v.quantile_values);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 6 pk
    | Some (8, Pbrt.Varint) -> begin
      summary_data_point_set_flags v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary_data_point" 8 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : summary_data_point)

let rec decode_pb_summary d =
  let v = default_summary () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      summary_set_data_points v (List.rev v.data_points);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      summary_set_data_points v ((decode_pb_summary_data_point (Pbrt.Decoder.nested d)) :: v.data_points);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "summary" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : summary)

let rec decode_pb_metric_data d = 
  let rec loop () = 
    let ret:metric_data = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "metric_data"
      | Some (5, _) -> (Gauge (decode_pb_gauge (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (7, _) -> (Sum (decode_pb_sum (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (9, _) -> (Histogram (decode_pb_histogram (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (10, _) -> (Exponential_histogram (decode_pb_exponential_histogram (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (11, _) -> (Summary (decode_pb_summary (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_metric d =
  let v = default_metric () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      metric_set_metadata v (List.rev v.metadata);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      metric_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      metric_set_description v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      metric_set_unit_ v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 3 pk
    | Some (5, Pbrt.Bytes) -> begin
      metric_set_data v (Gauge (decode_pb_gauge (Pbrt.Decoder.nested d)));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 5 pk
    | Some (7, Pbrt.Bytes) -> begin
      metric_set_data v (Sum (decode_pb_sum (Pbrt.Decoder.nested d)));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 7 pk
    | Some (9, Pbrt.Bytes) -> begin
      metric_set_data v (Histogram (decode_pb_histogram (Pbrt.Decoder.nested d)));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 9 pk
    | Some (10, Pbrt.Bytes) -> begin
      metric_set_data v (Exponential_histogram (decode_pb_exponential_histogram (Pbrt.Decoder.nested d)));
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 10 pk
    | Some (11, Pbrt.Bytes) -> begin
      metric_set_data v (Summary (decode_pb_summary (Pbrt.Decoder.nested d)));
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 11 pk
    | Some (12, Pbrt.Bytes) -> begin
      metric_set_metadata v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.metadata);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metric" 12 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : metric)

let rec decode_pb_scope_metrics d =
  let v = default_scope_metrics () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      scope_metrics_set_metrics v (List.rev v.metrics);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      scope_metrics_set_scope v (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "scope_metrics" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      scope_metrics_set_metrics v ((decode_pb_metric (Pbrt.Decoder.nested d)) :: v.metrics);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "scope_metrics" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      scope_metrics_set_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "scope_metrics" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : scope_metrics)

let rec decode_pb_resource_metrics d =
  let v = default_resource_metrics () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      resource_metrics_set_scope_metrics v (List.rev v.scope_metrics);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      resource_metrics_set_resource v (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "resource_metrics" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      resource_metrics_set_scope_metrics v ((decode_pb_scope_metrics (Pbrt.Decoder.nested d)) :: v.scope_metrics);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "resource_metrics" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      resource_metrics_set_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "resource_metrics" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : resource_metrics)

let rec decode_pb_metrics_data d =
  let v = default_metrics_data () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      metrics_data_set_resource_metrics v (List.rev v.resource_metrics);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      metrics_data_set_resource_metrics v ((decode_pb_resource_metrics (Pbrt.Decoder.nested d)) :: v.resource_metrics);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "metrics_data" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : metrics_data)

let rec decode_pb_data_point_flags d : data_point_flags = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> Data_point_flags_do_not_use
  | 1 -> Data_point_flags_no_recorded_value_mask
  | _ -> Pbrt.Decoder.malformed_variant "data_point_flags"
