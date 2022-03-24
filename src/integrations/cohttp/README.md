# Opentelemetry tracing for Cohttp_lwt servers

Wrap your server callback with `Opentelemetry_cohttp_lwt.trace`:

```ocaml
let my_server callback =
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8080))
    (Server.make 
      ~callback:(Opentelemetry_cohttp_lwt.trace ~service_name:"my-service" callback)
      ())
```
