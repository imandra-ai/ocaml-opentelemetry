(** Any kind of signal *)

open Common_

type t =
  | Span of Span.t
  | Metric of Metrics.t
  | Log of Log_record.t

let pp out = function
  | Span s -> Proto.Trace.pp_span out s
  | Metric m -> Proto.Metrics.pp_metric out m
  | Log l -> Proto.Logs.pp_log_record out l
