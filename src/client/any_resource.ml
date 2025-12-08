open Opentelemetry.Proto

(** A resource *)
type t =
  | R_metrics of Metrics.resource_metrics list
  | R_spans of Trace.resource_spans list
  | R_logs of Logs.resource_logs list

open struct
  let of_x_or_empty ?service_name ?attrs ~f l =
    if l = [] then
      []
    else
      [ f ?service_name ?attrs l ]
end

let of_logs ?service_name ?attrs logs : t =
  R_logs [ Util_resources.make_resource_logs ?service_name ?attrs logs ]

let of_logs_or_empty ?service_name ?attrs logs =
  of_x_or_empty ?service_name ?attrs ~f:of_logs logs

let of_spans ?service_name ?attrs spans : t =
  R_spans [ Util_resources.make_resource_spans ?service_name ?attrs spans ]

let of_spans_or_empty ?service_name ?attrs spans =
  of_x_or_empty ?service_name ?attrs ~f:of_spans spans

let of_metrics ?service_name ?attrs m : t =
  R_metrics [ Util_resources.make_resource_metrics ?service_name ?attrs m ]

let of_metrics_or_empty ?service_name ?attrs ms =
  of_x_or_empty ?service_name ?attrs ~f:of_metrics ms
