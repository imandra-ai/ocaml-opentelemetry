module Otel = Opentelemetry
module Otrace = Trace (* ocaml-trace *)
module TLS = Ambient_context_tls.Thread_local

type span_begin = {
  id: Otel.Span_id.t;
  start_time: int64;
  name: string;
  data: (string * Otrace.user_data) list;
  __FILE__: string;
  __LINE__: int;
  __FUNCTION__: string option;
  trace_id: Otel.Trace_id.t;
  scope: Otel.Scope.t;
  parent_id: Otel.Span_id.t option;
  parent_scope: Otel.Scope.t option;
}

(** Table indexed by ocaml-trace spans *)
module Active_span_tbl = Hashtbl.Make (struct
  include Int64

  let hash : t -> int = Hashtbl.hash
end)

(** Per-thread set of active spans. *)
module Active_spans = struct
  type t = { tbl: span_begin Active_span_tbl.t } [@@unboxed]

  let create () : t = { tbl = Active_span_tbl.create 32 }

  let tls : t TLS.t = TLS.create ()

  let[@inline] get () : t = TLS.get_or_create tls ~create
end

let otrace_of_otel (id : Otel.Span_id.t) : int64 =
  let bs = Otel.Span_id.to_bytes id in
  (* lucky that it coincides! *)
  assert (Bytes.length bs = 8);
  Bytes.get_int64_le bs 0

let otel_of_otrace (id : int64) : Otel.Span_id.t =
  let bs = Bytes.create 8 in
  Bytes.set_int64_le bs 0 id;
  Otel.Span_id.of_bytes bs

let spankind_of_string =
  let open Otel.Span in
  function
  | "INTERNAL" -> Span_kind_internal
  | "SERVER" -> Span_kind_server
  | "CLIENT" -> Span_kind_client
  | "PRODUCER" -> Span_kind_producer
  | "CONSUMER" -> Span_kind_consumer
  | _ -> Span_kind_unspecified

let otel_attrs_of_otrace_data (data : Otel.Span.key_value list) =
  let kind : Otel.Span.kind ref = ref Otel.Span.Span_kind_unspecified in
  let data =
    List.filter_map
      (function
        | name, `String v when name = "otrace.spankind" ->
          kind := spankind_of_string v;
          None
        | x -> Some x)
      data
  in
  !kind, data

let enter_span' ?explicit_parent ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name =
  let open Otel in
  let otel_id = Span_id.create () in
  let otrace_id = otrace_of_otel otel_id in

  let parent_scope = Scope.get_ambient_scope () in
  let trace_id =
    match parent_scope with
    | Some sc -> sc.trace_id
    | None -> Trace_id.create ()
  in
  let parent_id =
    match explicit_parent, parent_scope with
    | Some p, _ -> Some (otel_of_otrace p)
    | None, Some parent -> Some parent.span_id
    | None, None -> None
  in

  let new_scope =
    { Scope.span_id = otel_id; trace_id; events = []; attrs = [] }
  in

  let start_time = Timestamp_ns.now_unix_ns () in

  let sb =
    {
      id = otel_id;
      start_time;
      name;
      data;
      __FILE__;
      __LINE__;
      __FUNCTION__;
      trace_id;
      scope = new_scope;
      parent_id;
      parent_scope;
    }
  in

  let active_spans = Active_spans.get () in
  Active_span_tbl.add active_spans.tbl otrace_id sb;

  otrace_id, sb

let exit_span' otrace_id
    {
      id = otel_id;
      start_time;
      name;
      data;
      __FILE__;
      __LINE__;
      __FUNCTION__;
      trace_id;
      scope = _;
      parent_id;
      parent_scope = _;
    } =
  let open Otel in
  let active_spans = Active_spans.get () in
  Active_span_tbl.remove active_spans.tbl otrace_id;

  let end_time = Timestamp_ns.now_unix_ns () in

  let kind, attrs = otel_attrs_of_otrace_data data in

  let attrs =
    match __FUNCTION__ with
    | None ->
      [ "code.filepath", `String __FILE__; "code.lineno", `Int __LINE__ ]
      @ attrs
    | Some __FUNCTION__ ->
      let last_dot = String.rindex __FUNCTION__ '.' in
      let module_path = String.sub __FUNCTION__ 0 last_dot in
      let function_name =
        String.sub __FUNCTION__ (last_dot + 1)
          (String.length __FUNCTION__ - last_dot - 1)
      in
      [
        "code.filepath", `String __FILE__;
        "code.lineno", `Int __LINE__;
        "code.function", `String function_name;
        "code.namespace", `String module_path;
      ]
      @ attrs
  in
  Span.create ~kind ~trace_id ?parent:parent_id ~id:otel_id ~start_time
    ~end_time ~attrs name
  |> fst

let collector () : Trace.collector =
  let module M = struct
    let with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name cb =
      let otrace_id, sb =
        enter_span' ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name
      in

      Otel.Scope.with_ambient_scope sb.scope @@ fun () ->
      let rv = cb otrace_id in

      let otel_span = exit_span' otrace_id sb in
      Otel.Trace.emit [ otel_span ];

      rv

    let enter_manual_span ~(parent : Otrace.explicit_span option) ~flavor:_
        ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name : Otrace.explicit_span =
      let otrace_id, sb =
        match parent with
        | None -> enter_span' ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name
        | Some { span; _ } ->
          enter_span' ~explicit_parent:span ~__FUNCTION__ ~__FILE__ ~__LINE__
            ~data name
      in

      let active_spans = Active_spans.get () in
      Active_span_tbl.add active_spans.tbl otrace_id sb;

      Otrace.{ span = otrace_id; meta = Meta_map.empty }

    let exit_manual_span Otrace.{ span = otrace_id; _ } =
      let active_spans = Active_spans.get () in
      match Active_span_tbl.find_opt active_spans.tbl otrace_id with
      | None ->
        (* FIXME: some kind of error/debug logging *)
        ()
      | Some sb ->
        let otel_span = exit_span' otrace_id sb in
        Otel.Trace.emit [ otel_span ]

    let message ?span ~data:_ msg : unit =
      (* gather information from context *)
      let old_scope = Otel.Scope.get_ambient_scope () in
      let trace_id = Option.map (fun sc -> sc.Otel.Scope.trace_id) old_scope in

      let span_id =
        match span with
        | Some id -> Some (otel_of_otrace id)
        | None -> Option.map (fun sc -> sc.Otel.Scope.span_id) old_scope
      in

      let log = Otel.Logs.make_str ?trace_id ?span_id msg in
      Otel.Logs.emit [ log ]

    let shutdown () = ()

    let name_process _name = ()

    let name_thread _name = ()

    let counter_int name cur_val : unit =
      let m = Otel.Metrics.(gauge ~name [ int cur_val ]) in
      Otel.Metrics.emit [ m ]

    let counter_float name cur_val : unit =
      let m = Otel.Metrics.(gauge ~name [ float cur_val ]) in
      Otel.Metrics.emit [ m ]
  end in
  (module M)

let setup () = Trace.setup_collector @@ collector ()

let setup_with_otel_backend b : unit =
  Otel.Collector.set_backend b;
  setup ()
