open Common_
open Trace_core
module Span_tbl = Trace_subscriber.Span_tbl

module Buf_pool = struct
  type t = Buffer.t Rpool.t

  let create ?(max_size = 32) ?(buf_size = 256) () : t =
    Rpool.create ~max_size ~clear:Buffer.reset
      ~create:(fun () -> Buffer.create buf_size)
      ()
end

open struct
  let[@inline] time_us_of_time_ns (t : int64) : float =
    Int64.div t 1_000L |> Int64.to_float

  let[@inline] int64_of_trace_id_ (id : Trace_core.trace_id) : int64 =
    if id == Trace_core.Collector.dummy_trace_id then
      0L
    else
      Bytes.get_int64_le (Bytes.unsafe_of_string id) 0
end

let on_tracing_error = ref (fun s -> Printf.eprintf "%s\n%!" s)

type span_info = {
  tid: int;
  name: string;
  start_us: float;
  mutable data: (string * Sub.user_data) list;
      (* NOTE: thread safety: this is supposed to only be modified by the thread
that's running this (synchronous, stack-abiding) span. *)
}
(** Information we store about a span begin event, to emit a complete event when
    we meet the corresponding span end event *)

type t = {
  active: bool A.t;
  pid: int;
  spans: span_info Span_tbl.t;
  buf_pool: Buf_pool.t;
  exporter: Exporter.t;
}
(** Subscriber state *)

open struct
  let print_non_closed_spans_warning spans =
    let module Str_set = Set.Make (String) in
    let spans = Span_tbl.to_list spans in
    if spans <> [] then (
      !on_tracing_error
      @@ Printf.sprintf "trace-tef: warning: %d spans were not closed"
           (List.length spans);
      let names =
        List.fold_left
          (fun set (_, span) -> Str_set.add span.name set)
          Str_set.empty spans
      in
      Str_set.iter
        (fun name ->
          !on_tracing_error @@ Printf.sprintf "  span %S was not closed" name)
        names;
      flush stderr
    )
end

let close (self : t) : unit =
  if A.exchange self.active false then (
    print_non_closed_spans_warning self.spans;
    self.exporter.close ()
  )

let[@inline] active self = A.get self.active

let[@inline] flush (self : t) : unit = self.exporter.flush ()

let create ?(buf_pool = Buf_pool.create ()) ~pid ~exporter () : t =
  { active = A.make true; exporter; buf_pool; pid; spans = Span_tbl.create () }

module Callbacks = struct
  type st = t

  let on_init _ ~time_ns:_ = ()

  let on_shutdown (self : st) ~time_ns:_ = close self

  let on_name_process (self : st) ~time_ns:_ ~tid:_ ~name : unit =
    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_name_process ~pid:self.pid ~name buf;
    self.exporter.on_json buf

  let on_name_thread (self : st) ~time_ns:_ ~tid ~name : unit =
    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_name_thread buf ~pid:self.pid ~tid ~name;
    self.exporter.on_json buf

  (* add function name, if provided, to the metadata *)
  let add_fun_name_ fun_name data : _ list =
    match fun_name with
    | None -> data
    | Some f -> ("function", Sub.U_string f) :: data

  let[@inline] on_enter_span (self : st) ~__FUNCTION__:fun_name ~__FILE__:_
      ~__LINE__:_ ~time_ns ~tid ~data ~name span : unit =
    let time_us = time_us_of_time_ns @@ time_ns in
    let data = add_fun_name_ fun_name data in
    let info = { tid; name; start_us = time_us; data } in
    (* save the span so we find it at exit *)
    Span_tbl.add self.spans span info

  let on_exit_span (self : st) ~time_ns ~tid:_ span : unit =
    let time_us = time_us_of_time_ns @@ time_ns in

    match Span_tbl.find_exn self.spans span with
    | exception Not_found ->
      !on_tracing_error
        (Printf.sprintf "trace-tef: error: cannot find span %Ld" span)
    | { tid; name; start_us; data } ->
      Span_tbl.remove self.spans span;
      let@ buf = Rpool.with_ self.buf_pool in
      Writer.emit_duration_event buf ~pid:self.pid ~tid ~name ~start:start_us
        ~end_:time_us ~args:data;

      self.exporter.on_json buf

  let on_add_data (self : st) ~data span =
    if data <> [] then (
      try
        let info = Span_tbl.find_exn self.spans span in
        info.data <- List.rev_append data info.data
      with Not_found ->
        !on_tracing_error
          (Printf.sprintf "trace-tef: error: cannot find span %Ld" span)
    )

  let on_message (self : st) ~time_ns ~tid ~span:_ ~data msg : unit =
    let time_us = time_us_of_time_ns @@ time_ns in
    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_instant_event buf ~pid:self.pid ~tid ~name:msg ~ts:time_us
      ~args:data;
    self.exporter.on_json buf

  let on_counter (self : st) ~time_ns ~tid ~data:_ ~name n : unit =
    let time_us = time_us_of_time_ns @@ time_ns in
    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_counter buf ~pid:self.pid ~name ~tid ~ts:time_us n;
    self.exporter.on_json buf

  let on_enter_manual_span (self : st) ~__FUNCTION__:fun_name ~__FILE__:_
      ~__LINE__:_ ~time_ns ~tid ~parent:_ ~data ~name ~flavor ~trace_id _span :
      unit =
    let time_us = time_us_of_time_ns @@ time_ns in

    let data = add_fun_name_ fun_name data in
    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_manual_begin buf ~pid:self.pid ~tid ~name
      ~id:(int64_of_trace_id_ trace_id)
      ~ts:time_us ~args:data ~flavor;
    self.exporter.on_json buf

  let on_exit_manual_span (self : st) ~time_ns ~tid ~name ~data ~flavor
      ~trace_id (_ : span) : unit =
    let time_us = time_us_of_time_ns @@ time_ns in

    let@ buf = Rpool.with_ self.buf_pool in
    Writer.emit_manual_end buf ~pid:self.pid ~tid ~name
      ~id:(int64_of_trace_id_ trace_id)
      ~ts:time_us ~flavor ~args:data;
    self.exporter.on_json buf

  let on_extension_event _ ~time_ns:_ ~tid:_ _ev = ()
end

let subscriber (self : t) : Sub.t =
  Sub.Subscriber.Sub { st = self; callbacks = (module Callbacks) }
