open Cohttp
open Cohttp_lwt

type ('conn, 'body) callback =
  'conn (* Cohttp_lwt_unix.Server.conn *)
  -> Request.t
  -> 'body (* Cohttp_lwt.Body.t *)
  -> (Response.t * 'body) Lwt.t

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


let trace ~service_name (callback : ('conn, 'body) callback ) : ('conn, 'body) callback =
  fun conn req body ->
  let trace_id =
    Header.get (Request.headers req) "trace-id"
    |> Option.map (fun s ->
           s |> Bytes.of_string |> Opentelemetry.Trace_id.of_bytes )
  in
  let parent_id =
    Header.get (Request.headers req) "parent-id"
    |> Option.map (fun s ->
           s |> Bytes.of_string |> Opentelemetry.Span_id.of_bytes )
  in
  let open Lwt.Syntax in
  Opentelemetry_lwt.Trace.with_
    ~service_name
    "request"
    ~kind:Span_kind_server
    ~attrs:(span_attrs req)
    ?parent:parent_id
    ?trace_id
    (fun scope ->
      let* (res, body) = callback conn req body in
      Opentelemetry.Trace.add_attrs scope (fun () ->
          let code = Response.status res in
          let code = Code.code_of_status code in
          [ ("http.status_code", `Int code) ]) ;
      Lwt.return (res, body) )
