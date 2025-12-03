(** Group signals into [resource_xxx] objects *)

open Common_

let make_resource_logs (logs : Proto.Logs.log_record list) :
    Proto.Logs.resource_logs =
  let attributes = OTEL.Globals.mk_attributes () in
  let resource = Proto.Resource.make_resource ~attributes () in
  let ll =
    Proto.Logs.make_scope_logs ~scope:OTEL.Globals.instrumentation_library
      ~log_records:logs ()
  in
  Proto.Logs.make_resource_logs ~resource ~scope_logs:[ ll ] ()

let make_resource_spans ?service_name ?attrs spans : Proto.Trace.resource_spans
    =
  let ils =
    Proto.Trace.make_scope_spans ~scope:OTEL.Globals.instrumentation_library
      ~spans ()
  in
  let attributes = OTEL.Globals.mk_attributes ?service_name ?attrs () in
  let resource = Proto.Resource.make_resource ~attributes () in
  Proto.Trace.make_resource_spans ~resource ~scope_spans:[ ils ] ()

(** Aggregate metrics into a {!Proto.Metrics.resource_metrics} *)
let make_resource_metrics ?service_name ?attrs (l : OTEL.Metrics.t list) :
    Proto.Metrics.resource_metrics =
  let open Proto.Metrics in
  let lm =
    make_scope_metrics ~scope:OTEL.Globals.instrumentation_library ~metrics:l ()
  in
  let attributes = OTEL.Globals.mk_attributes ?service_name ?attrs () in
  let resource = Proto.Resource.make_resource ~attributes () in
  Proto.Metrics.make_resource_metrics ~scope_metrics:[ lm ] ~resource ()
