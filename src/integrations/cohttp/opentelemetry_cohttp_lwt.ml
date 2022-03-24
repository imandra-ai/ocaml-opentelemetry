module Otel = Opentelemetry
module Otel_lwt = Opentelemetry_lwt
open Cohttp
open Cohttp_lwt

module Server : sig
  (** Trace requests to a Cohttp server.

      Use it like this:

          let my_server callback =
            let callback =
              Opentelemetry_cohttp_lwt.Server.trace ~service_name:"my-service" callback in
            Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8080))
              (Server.make () ~callback)
   *)
  val trace :
    service_name:string ->
    ?attrs:Otel.Span.key_value list ->
    ('conn -> Request.t -> 'body -> (Response.t * 'body) Lwt.t) ->
    'conn -> Request.t -> 'body -> (Response.t * 'body) Lwt.t
end = struct
  let span_attrs (req : Request.t) =
    let meth = req |> Request.meth |> Code.string_of_method in
    let referer = Header.get (Request.headers req) "referer" in
    let host = Header.get (Request.headers req) "host" in
    let ua = Header.get (Request.headers req) "user-agent" in
    let uri = Request.uri req in
    List.concat
      [ [ ("http.method", `String meth) ]
      ; (match host with None -> [] | Some h -> [ ("http.host", `String h) ])
      ; [ ("http.url", `String (Uri.to_string uri)) ]
      ; ( match ua with
          | None ->
             []
          | Some ua ->
             [ ("http.user_agent", `String ua) ] )
      ; ( match referer with
          | None ->
             []
          | Some r ->
             [ ("http.request.header.referer", `String r) ] )
      ]

  let trace_context_of_headers req =
    let module Traceparent = Otel.Trace_context.Traceparent in
    match Header.get (Request.headers req) Traceparent.name with
    | None -> None, None
    | Some v ->
       (match Traceparent.of_value v with
        | Ok (trace_id, parent_id) -> (Some trace_id, Some parent_id)
        | Error _ -> None, None)

  let trace ~service_name ?(attrs=[]) callback =
    fun conn req body ->
    let trace_id, parent_id = trace_context_of_headers req in
    let open Lwt.Syntax in
    Otel_lwt.Trace.with_
      ~service_name
      "request"
      ~kind:Span_kind_server
      ~attrs:(attrs @ span_attrs req)
      ?parent:parent_id
      ?trace_id
      (fun scope ->
        let* (res, body) = callback conn req body in
        Otel.Trace.add_attrs scope (fun () ->
            let code = Response.status res in
            let code = Code.code_of_status code in
            [ ("http.status_code", `Int code) ]) ;
        Lwt.return (res, body) )
end
