(** Semantic conventions.

    {{:https://opentelemetry.io/docs/specs/semconv/}
     https://opentelemetry.io/docs/specs/semconv/} *)

module Attributes = struct
  module Process = struct
    module Runtime = struct
      let name = "process.runtime.name"

      let version = "process.runtime.version"

      let description = "process.runtime.description"
    end
  end

  (** https://opentelemetry.io/docs/specs/semconv/attributes-registry/code/ *)
  module Code = struct
    (** Int *)
    let column = "code.column"

    let filepath = "code.filepath"

    let function_ = "code.function"

    (** int *)
    let line = "code.lineno"

    let namespace = "code.namespace"

    let stacktrace = "code.stacktrace"
  end

  module Service = struct
    let name = "service.name"

    let namespace = "service.namespace"

    let instance_id = "service.instance.id"

    let version = "service.version"
  end

  module HTTP = struct
    let error_type = "error.type"

    let request_method = "http.request.method"

    let route = "http.route"

    let url_full = "url.full"

    (** HTTP status code, int *)
    let response_status_code = "http.response.status_code"

    let server_address = "server.address"

    let server_port = "server.port"

    (** http or https *)
    let url_scheme = "url.scheme"
  end

  (** https://github.com/open-telemetry/semantic-conventions/blob/main/docs/resource/host.md
  *)
  module Host = struct
    let id = "host.id"

    let name = "host.name"

    let type_ = "host.type"

    let arch = "host.arch"

    let ip = "host.ip"

    let mac = "host.mac"

    let image_id = "host.image.id"

    let image_name = "host.image.name"

    let image_version = "host.image.version"
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

  (** https://opentelemetry.io/docs/specs/semconv/http/ *)
  module HTTP = struct
    module Server = struct
      let request_duration = "http.server.request.duration"

      let active_requests = "http.server.active_requests"

      (** Histogram *)
      let request_body_size = "http.server.request.body.size"

      (** Histogram *)
      let response_body_size = "http.server.response.body.size"
    end

    module Client = struct
      let request_duration = "http.client.request.duration"

      (** Histogram *)
      let request_body_size = "http.client.request.body.size"

      (** Histogram *)
      let response_body_size = "http.client.response.body.size"
    end
  end
end
