open Opentelemetry.Proto

(** A resource *)
type t =
  | R_metrics of Metrics.resource_metrics list
  | R_spans of Trace.resource_spans list
  | R_logs of Logs.resource_logs list

let of_logs logs : t = R_logs [ Util_resources.make_resource_logs logs ]

open struct
  let of_x_or_empty ~f l =
    if l = [] then
      []
    else
      [ f l ]
end

let of_logs_or_empty logs = of_x_or_empty ~f:of_logs logs

let of_spans spans : t = R_spans [ Util_resources.make_resource_spans spans ]

let of_spans_or_empty spans = of_x_or_empty ~f:of_spans spans

let of_metrics m : t = R_metrics [ Util_resources.make_resource_metrics m ]

let of_metrics_or_empty ms = of_x_or_empty ~f:of_metrics ms
