open Common_
module Conv = Conv
open Conv

let on_internal_error =
  ref (fun msg -> Printf.eprintf "error in Opentelemetry_trace: %s\n%!" msg)

module Extensions = struct
  type Otrace.extension_event +=
    | Ev_link_span of Otrace.explicit_span * OTEL.Span_ctx.t
    | Ev_record_exn of {
        sp: Otrace.explicit_span;
        exn: exn;
        bt: Printexc.raw_backtrace;
      }
    | Ev_set_span_kind of Otrace.explicit_span * OTEL.Span_kind.t
end

open Extensions

(* use the fast, thread safe span table that relies on picos. *)
module Span_tbl = Trace_subscriber.Span_tbl

module Internal = struct
  type span_begin = { span: OTEL.Span.t } [@@unboxed]

  module Active_span_tbl = Span_tbl

  type state = { tbl: span_begin Active_span_tbl.t } [@@unboxed]

  let create_state () : state = { tbl = Active_span_tbl.create () }

  (** key to access a OTEL span (the current span) from a Trace_core
      explicit_span *)
  let k_explicit_span : OTEL.Span.t Otrace.Meta_map.key =
    Otrace.Meta_map.Key.create ()

  let otrace_of_otel (id : OTEL.Span_id.t) : int64 =
    let bs = OTEL.Span_id.to_bytes id in
    (* lucky that it coincides! *)
    assert (Bytes.length bs = 8);
    Bytes.get_int64_le bs 0

  let enter_span' (self : state)
      ?(explicit_parent : Otrace.explicit_span_ctx option) ~__FUNCTION__
      ~__FILE__ ~__LINE__ ~data name =
    let open OTEL in
    let otel_id = Span_id.create () in
    let otrace_id = otrace_of_otel otel_id in

    let implicit_parent = OTEL.Ambient_span.get () in

    let trace_id, parent_id =
      match explicit_parent, implicit_parent with
      | Some p, _ ->
        let trace_id = p.trace_id |> Conv.trace_id_to_otel in
        let parent_id =
          try
            let sb = Active_span_tbl.find_exn self.tbl p.span in
            Some (OTEL.Span.id sb.span)
          with Not_found -> None
        in
        trace_id, parent_id
      | None, Some p -> Span.trace_id p, Some (Span.id p)
      | None, None -> Trace_id.create (), None
    in

    let attrs =
      ("code.filepath", `String __FILE__)
      :: ("code.lineno", `Int __LINE__)
      :: data
    in

    let start_time = Timestamp_ns.now_unix_ns () in
    let span : OTEL.Span.t =
      OTEL.Span.make ?parent:parent_id ~trace_id ~id:otel_id ~attrs name
        ~start_time ~end_time:start_time
    in

    let sb = { span } in

    (match __FUNCTION__ with
    | Some __FUNCTION__ when OTEL.Span.is_not_dummy span ->
      let last_dot = String.rindex __FUNCTION__ '.' in
      let module_path = String.sub __FUNCTION__ 0 last_dot in
      let function_name =
        String.sub __FUNCTION__ (last_dot + 1)
          (String.length __FUNCTION__ - last_dot - 1)
      in
      Span.add_attrs span
        [
          "code.function", `String function_name;
          "code.namespace", `String module_path;
        ]
    | _ -> ());

    Active_span_tbl.add self.tbl otrace_id sb;

    otrace_id, sb

  let exit_span_ { span } : OTEL.Span.t =
    let open OTEL in
    let end_time = Timestamp_ns.now_unix_ns () in
    Proto.Trace.span_set_end_time_unix_nano span end_time;
    span

  let exit_span' (self : state) otrace_id otel_span_begin =
    Active_span_tbl.remove self.tbl otrace_id;
    exit_span_ otel_span_begin

  (** Find the OTEL span corresponding to this Trace span *)
  let exit_span_from_id (self : state) otrace_id =
    match Active_span_tbl.find_exn self.tbl otrace_id with
    | exception Not_found -> None
    | otel_span_begin ->
      Active_span_tbl.remove self.tbl otrace_id;
      Some (exit_span_ otel_span_begin)

  let[@inline] get_span_ (span : Otrace.explicit_span) : OTEL.Span.t option =
    Otrace.Meta_map.find k_explicit_span span.meta
end

module type COLLECTOR_ARG = sig
  val exporter : OTEL.Exporter.t
end

module Make_collector (A : COLLECTOR_ARG) = struct
  open Internal

  let exporter = A.exporter

  let state = create_state ()

  let with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name cb =
    let otrace_id, sb =
      enter_span' state ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name
    in

    match
      let@ () = OTEL.Ambient_span.with_ambient sb.span in
      cb otrace_id
    with
    | res ->
      let otel_span = exit_span' state otrace_id sb in
      OTEL.Exporter.send_trace exporter [ otel_span ];
      res
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in

      OTEL.Span.record_exception sb.span e bt;
      let otel_span = exit_span' state otrace_id sb in
      OTEL.Exporter.send_trace exporter [ otel_span ];

      Printexc.raise_with_backtrace e bt

  let enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name : Trace_core.span
      =
    let otrace_id, _sb =
      enter_span' state ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name
    in
    (* NOTE: we cannot enter ambient scope in a disjoint way
         with the exit, because we only have [Ambient_context.with_binding],
         no [set_binding] *)
    otrace_id

  let exit_span otrace_id =
    match exit_span_from_id state otrace_id with
    | None -> ()
    | Some otel_span -> OTEL.Exporter.send_trace exporter [ otel_span ]

  let enter_manual_span ~(parent : Otrace.explicit_span_ctx option) ~flavor:_
      ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name : Otrace.explicit_span =
    let otrace_id, sb =
      match parent with
      | None -> enter_span' state ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name
      | Some parent ->
        enter_span' state ~explicit_parent:parent ~__FUNCTION__ ~__FILE__
          ~__LINE__ ~data name
    in

    Active_span_tbl.add state.tbl otrace_id sb;

    {
      Otrace.span = otrace_id;
      trace_id = trace_id_of_otel (OTEL.Span.trace_id sb.span);
      meta = Otrace.Meta_map.(empty |> add k_explicit_span sb.span);
    }

  let exit_manual_span { Otrace.span = otrace_id; _ } =
    match Active_span_tbl.find_exn state.tbl otrace_id with
    | exception Not_found ->
      !on_internal_error (spf "no active span with ID %Ld" otrace_id)
    | sb ->
      let otel_span = exit_span' state otrace_id sb in
      OTEL.Exporter.send_trace exporter [ otel_span ]

  let add_data_to_span otrace_id data =
    match Active_span_tbl.find_exn state.tbl otrace_id with
    | exception Not_found ->
      !on_internal_error (spf "no active span with ID %Ld" otrace_id)
    | sb -> OTEL.Span.add_attrs sb.span data

  let add_data_to_manual_span (span : Otrace.explicit_span) data : unit =
    match get_span_ span with
    | None ->
      !on_internal_error (spf "manual span does not a contain an OTEL scope")
    | Some span -> OTEL.Span.add_attrs span data

  let message ?span ~data:_ msg : unit =
    (* gather information from context *)
    let old_span = OTEL.Ambient_span.get () in
    let trace_id = Option.map OTEL.Span.trace_id old_span in

    let span_id =
      match span with
      | Some id -> Some (span_id_to_otel id)
      | None -> Option.map OTEL.Span.id old_span
    in

    let log = OTEL.Log_record.make_str ?trace_id ?span_id msg in
    OTEL.Exporter.send_logs exporter [ log ]

  let shutdown () = ()

  let name_process _name = ()

  let name_thread _name = ()

  let counter_int ~data:attrs name cur_val : unit =
    let m = OTEL.Metrics.(gauge ~name [ int ~attrs cur_val ]) in
    OTEL.Exporter.send_metrics exporter [ m ]

  let counter_float ~data:attrs name cur_val : unit =
    let m = OTEL.Metrics.(gauge ~name [ float ~attrs cur_val ]) in
    OTEL.Exporter.send_metrics exporter [ m ]

  let extension_event = function
    | Ev_link_span (sp1, sc2) ->
      (match get_span_ sp1 with
      | Some sc1 -> OTEL.Span.add_links sc1 [ OTEL.Span_link.of_span_ctx sc2 ]
      | _ -> !on_internal_error "could not find scope for OTEL span")
    | Ev_set_span_kind (sp, k) ->
      (match get_span_ sp with
      | None -> !on_internal_error "could not find scope for OTEL span"
      | Some sc -> OTEL.Span.set_kind sc k)
    | Ev_record_exn { sp; exn; bt } ->
      (match get_span_ sp with
      | None -> !on_internal_error "could not find scope for OTEL span"
      | Some sc -> OTEL.Span.record_exception sc exn bt)
    | _ -> ()
end

let collector_of_exporter (exp : OTEL.Exporter.t) : Trace_core.collector =
  let module M = Make_collector (struct
    let exporter = exp
  end) in
  (module M : Trace_core.Collector.S)

let link_span_to_otel_ctx (sp1 : Otrace.explicit_span) (sp2 : OTEL.Span_ctx.t) :
    unit =
  if Otrace.enabled () then Otrace.extension_event @@ Ev_link_span (sp1, sp2)

(*
let link_spans (sp1 : Otrace.explicit_span) (sp2 : Otrace.explicit_span) : unit
    =
  if Otrace.enabled () then Otrace.extension_event @@ Ev_link_span (sp1, sp2)
  *)

let set_span_kind sp k : unit =
  if Otrace.enabled () then Otrace.extension_event @@ Ev_set_span_kind (sp, k)

let record_exception sp exn bt : unit =
  if Otrace.enabled () then
    Otrace.extension_event @@ Ev_record_exn { sp; exn; bt }

(** Collector that forwards to the {b currently installed} OTEL exporter. *)
let collector_main_otel_exporter () : Otrace.collector =
  collector_of_exporter OTEL.Main_exporter.dynamic_forward_to_main_exporter

let (collector
     [@deprecated "use collector_of_exporter or collector_main_otel_exporter"])
    =
  collector_main_otel_exporter

let setup () = Otrace.setup_collector @@ collector_main_otel_exporter ()

let setup_with_otel_exporter exp : unit =
  let coll = collector_of_exporter exp in
  OTEL.Main_exporter.set exp;
  Otrace.setup_collector coll

let setup_with_otel_backend = setup_with_otel_exporter

module Well_known = struct end
