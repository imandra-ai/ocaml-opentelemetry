
(** Traces.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal} the spec} *)
module Trace = struct
  include Trace_types
  include Trace_pp
  include Trace_pb
end

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  include Metrics_types
  include Metrics_pp
  include Metrics_pb
end

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

(*
module Span = Span
module Timestamp = Timestamp
   *)

(** Collector types

    These types are used by backend implementations, to send events to
    collectors such as Jaeger.

    Note: most users will not need to touch this module *)
module Collector = struct

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

  (** Collector client interface. *)
  module type BACKEND = sig

    val send_trace : Trace_service.export_trace_service_request -> unit

    val send_metrics : Metrics_service.export_metrics_service_request -> unit

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  let backend : backend option ref = ref None

end

