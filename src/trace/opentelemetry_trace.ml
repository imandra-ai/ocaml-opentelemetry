module Otel = Opentelemetry
module TLS = Otel.Thread_local

type span = Trace.span

(** Table indexed by Trace spans *)
module Span_tbl = Hashtbl.Make (struct
  include Int64

  let hash : t -> int = Hashtbl.hash
end)

(** Per-thread set of active spans. *)
module Active_spans = struct
  type span_begin = {
    span_id: Otel.Span_id.t;
    start_time: int64;
    name: string;
    data: (string * Trace.user_data) list;
    __FILE__: string;
    __LINE__: int;
    new_scope: Otel.Scope.t;
    old_scope: Otel.Scope.t option;
  }
  (** Information we get at the beginning of the span *)

  type t = { tbl: span_begin Span_tbl.t } [@@unboxed]
  (** Storage for active spans *)

  let create () : t = { tbl = Span_tbl.create 8 }

  let tls : t TLS.t = TLS.create ()

  let[@inline] get () : t = TLS.get_or_create tls ~create
end

let conv_span_to_i64 (id : Otel.Span_id.t) : int64 =
  let bs = Otel.Span_id.to_bytes id in
  (* lucky that it coincides! *)
  assert (Bytes.length bs = 8);
  Bytes.get_int64_le bs 0

let span_of_i64 (id : int64) : Otel.Span_id.t =
  let bs = Bytes.create 8 in
  Bytes.set_int64_le bs 0 id;
  Otel.Span_id.of_bytes bs

let collector () : Trace.collector =
  let module M = struct
    let enter_span ?__FUNCTION__:_ ~__FILE__ ~__LINE__ ~data name : span =
      let span_id = Otel.Span_id.create () in
      let span = conv_span_to_i64 span_id in

      let start_time = Otel.Timestamp_ns.now_unix_ns () in

      let old_scope = Otel.Scope.get_surrounding () in
      let trace_id =
        match old_scope with
        | None -> Otel.Trace_id.create ()
        | Some sc -> sc.trace_id
      in

      let new_scope =
        match old_scope with
        | None -> { Otel.Scope.span_id; trace_id; events = []; attrs = [] }
        | Some sc -> sc
      in
      TLS.set Otel.Scope._global_scope new_scope;

      let active_spans = Active_spans.get () in
      Span_tbl.add active_spans.tbl span
        {
          span_id;
          start_time;
          __FILE__;
          __LINE__;
          old_scope;
          new_scope;
          name;
          data;
        };

      span

    let exit_span (span : span) : unit =
      let active_spans = Active_spans.get () in
      match Span_tbl.find_opt active_spans.tbl span with
      | None -> () (* TODO: log warning *)
      | Some
          {
            span_id;
            start_time;
            name;
            __FILE__;
            __LINE__;
            new_scope;
            old_scope;
            data;
          } ->
        let end_time = Otel.Timestamp_ns.now_unix_ns () in

        (* restore previous scope *)
        (match old_scope with
        | None -> TLS.remove Otel.Scope._global_scope
        | Some sc -> TLS.set Otel.Scope._global_scope sc);

        let o_span : Otel.Span.t =
          let attrs =
            [ "file", `String __FILE__; "line", `Int __LINE__ ] @ data
          in
          Otel.Span.create ~trace_id:new_scope.trace_id ~id:span_id ~start_time
            ~end_time ~attrs name
          |> fst
        in

        Otel.Trace.emit [ o_span ];

        ()

    let message ?span ~data:_ msg : unit =
      (* gather information from context *)
      let old_scope = Otel.Scope.get_surrounding () in
      let trace_id = Option.map (fun sc -> sc.Otel.Scope.trace_id) old_scope in

      let span_id =
        match span with
        | Some id -> Some (span_of_i64 id)
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
