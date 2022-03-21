
(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

(* TODO *)

open Opentelemetry

let[@inline] (let@) f x = f x

let debug_ = ref (try bool_of_string @@ Sys.getenv "DEBUG" with _ -> false)

let default_url = "http://localhost:4318"
let url = ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)
let get_url () = !url
let set_url s = url := s

let lock_ : (unit -> unit) ref = ref ignore
let unlock_ : (unit -> unit) ref = ref ignore
let set_mutex ~lock ~unlock : unit =
  lock_ := lock;
  unlock_ := unlock

module Config = struct
  type t = {
    debug: bool;
    url: string;
    batch_traces: int option;
    batch_metrics: int option;
    thread: bool;
  }

  let pp out self =
    let ppiopt = Format.pp_print_option Format.pp_print_int in
    let {debug; url; batch_traces; batch_metrics; thread} = self in
    Format.fprintf out "{@[ debug=%B;@ url=%S;@ \
                        batch_traces=%a;@ batch_metrics=%a;@ thread=%B @]}"
      debug url ppiopt batch_traces ppiopt batch_metrics
      thread

  let make
      ?(debug= !debug_)
      ?(url= get_url())
      ?(batch_traces=Some 400)
      ?(batch_metrics=None)
      ?(thread=true)
      () : t =
    { debug; url; batch_traces; batch_metrics; thread; }
end

(* critical section for [f()] *)
let[@inline] with_lock_ f =
  !lock_();
  Fun.protect ~finally:!unlock_ f

let[@inline] with_mutex_ m f =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let _init_curl = lazy (
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  at_exit Curl.global_cleanup;
)

type error = [
  | `Status of int * Opentelemetry.Proto.Status.status
  | `Failure of string
]

let report_err_ = function
  | `Failure msg ->
    Format.eprintf "@[<2>opentelemetry: export failed: %s@]@." msg
  | `Status (code, status) ->
    Format.eprintf "@[<2>opentelemetry: export failed with@ http code=%d@ status %a@]@."
      code Proto.Status.pp_status status

module type CURL = sig
  val send : path:string -> decode:(Pbrt.Decoder.t -> 'a) -> string -> ('a, error) result
  val cleanup : unit -> unit
end

(* create a curl client *)
module Curl() : CURL = struct
  open Opentelemetry.Proto
  let() = Lazy.force _init_curl

  let buf_res = Buffer.create 256

  (* TODO: use Curl.Multi, etc. instead? *)

  (* http client *)
  let curl : Curl.t = Curl.init ()

  let cleanup () = Curl.cleanup curl

  (* TODO: use Curl multi *)

  (* send the content to the remote endpoint/path *)
  let send ~path ~decode (bod:string) : ('a, error) result =
    Curl.reset curl;
    if !debug_ then Curl.set_verbose curl true;
    Curl.set_url curl (!url ^ path);
    Curl.set_httppost curl [];
    Curl.set_httpheader curl ["Content-Type: application/x-protobuf"];
    (* write body *)
    Curl.set_post curl true;
    Curl.set_postfieldsize curl (String.length bod);
    Curl.set_readfunction curl
      begin
        let i = ref 0 in
        (fun n ->
           if !debug_ then Printf.eprintf "curl asks for %d bytes\n%!" n;
           let len = min n (String.length bod - !i) in
           let s = String.sub bod !i len in
           if !debug_ then Printf.eprintf "gave curl %d bytes\n%!" len;
           i := !i + len;
           s)
      end;
    (* read result's body *)
    Buffer.clear buf_res;
    Curl.set_writefunction curl
      (fun s -> Buffer.add_string buf_res s; String.length s);
    try
      match Curl.perform curl with
      | () ->
        let code = Curl.get_responsecode curl in
        if !debug_ then Printf.eprintf "result body: %S\n%!" (Buffer.contents buf_res);
        let dec = Pbrt.Decoder.of_string (Buffer.contents buf_res) in
        if code >= 200 && code < 300 then (
          let res = decode dec in
          Ok res
        ) else (
          let status = Status.decode_status dec in
          Error (`Status (code, status))
        )
      | exception Curl.CurlException (_, code, msg) ->
        let status = Status.default_status
            ~code:(Int32.of_int code) ~message:(Bytes.unsafe_of_string msg) () in
        Error(`Status (code, status))
    with e -> Error (`Failure (Printexc.to_string e))
end

module type PUSH = sig
  type elt
  val push : elt -> unit
  val is_empty : unit -> bool
  val is_big_enough : unit -> bool
  val pop_iter_all : (elt -> unit) -> unit
end

(* queue of fixed size *)
module FQueue : sig
  type 'a t
  val create : dummy:'a -> int -> 'a t
  val size : _ t -> int
  val push : 'a t -> 'a -> unit
  val pop_iter_all : 'a t -> ('a -> unit) -> unit
end = struct
  type 'a t = {
    arr: 'a array;
    mutable i: int;
  }

  let create ~dummy n : _ t =
    assert (n >= 1);
    { arr=Array.make n dummy;
      i=0;
    }

  let[@inline] size self = self.i

  let push (self:_ t) x : unit =
    assert (self.i < Array.length self.arr);
    self.arr.(self.i) <- x;
    self.i <- 1 + self.i

  let pop_iter_all (self: _ t) f =
    for j=0 to self.i-1 do
      f self.arr.(j)
    done;
    self.i <- 0
end

(* generate random IDs *)
module Gen_ids() = struct
  let rand_ = Random.State.make_self_init()

  let rand_bytes_8 () : bytes =
    let@() = with_lock_ in
    let b = Bytes.create 8 in
    for i=0 to 1 do
      let r = Random.State.bits rand_ in (* 30 bits, of which we use 24 *)
      Bytes.set b (i*3) (Char.chr (r land 0xff));
      Bytes.set b (i*3+1) (Char.chr (r lsr 8 land 0xff));
      Bytes.set b (i*3+2) (Char.chr (r lsr 16 land 0xff));
    done;
    let r = Random.State.bits rand_ in
    Bytes.set b 6 (Char.chr (r land 0xff));
    Bytes.set b 7 (Char.chr (r lsr 8 land 0xff));
    b

  let rand_bytes_16 () : bytes =
    let@() = with_lock_ in
    let b = Bytes.create 16 in
    for i=0 to 4 do
      let r = Random.State.bits rand_ in (* 30 bits, of which we use 24 *)
      Bytes.set b (i*3) (Char.chr (r land 0xff));
      Bytes.set b (i*3+1) (Char.chr (r lsr 8 land 0xff));
      Bytes.set b (i*3+2) (Char.chr (r lsr 16 land 0xff));
    done;
    let r = Random.State.bits rand_ in
    Bytes.set b 15 (Char.chr (r land 0xff)); (* last byte *)
    b
end

(** Callback for when an event is properly sent to the collector *)
type over_cb = unit -> unit

(** An emitter. This is used by {!Backend} below to forward traces/metrics/…
    from the program to whatever collector client we have. *)
module type EMITTER = sig
  open Opentelemetry.Proto

  val push_trace : Trace.resource_spans list -> over:over_cb -> unit
  val push_metrics : Metrics.resource_metrics list -> over:over_cb -> unit

  val cleanup : unit -> unit
end

type 'a push = (module PUSH with type elt = 'a)
type on_full_cb = (unit -> unit)

(* make a "push" object, along with a setter for a callback to call when
   it's ready to emit a batch *)
let mk_push (type a) ?batch () : (module PUSH with type elt = a) * (on_full_cb -> unit) =
  let on_full: on_full_cb ref = ref ignore in
  let push =
    match batch with
    | None ->
      let r = ref None in
      let module M = struct
        type elt = a
        let is_empty () = !r == None
        let is_big_enough () = !r != None
        let push x =
          r := Some x; !on_full()
        let pop_iter_all f = Option.iter f !r; r := None
      end in
      (module M : PUSH with type elt = a)

    | Some n ->
      let q = FQueue.create ~dummy:(Obj.magic 0) (2 * n) in
      let module M = struct
        type elt = a
        let is_empty () = FQueue.size q = 0
        let is_big_enough () = FQueue.size q >= n
        let push x =
          FQueue.push q x;
          if FQueue.size q > n then (
            !on_full()
          )
        let pop_iter_all f = FQueue.pop_iter_all q f
      end in
      (module M : PUSH with type elt = a)

  in
  push, ((:=) on_full)

let mk_emitter ~(config:Config.t) () : (module EMITTER) =
  let open Proto in

  let continue = ref true in

  let ((module E_trace) : (Trace.resource_spans list * over_cb) push), on_trace_full =
    mk_push ?batch:config.batch_traces () in
  let ((module E_metrics) : (Metrics.resource_metrics list * over_cb) push), on_metrics_full =
    mk_push ?batch:config.batch_metrics () in

  let encoder = Pbrt.Encoder.create() in

  let emit_metrics (module C:CURL) (l:(Metrics.resource_metrics list*over_cb) list) =
    Pbrt.Encoder.reset encoder;
    let resource_metrics =
      List.fold_left (fun acc (l,_) -> List.rev_append l acc) [] l in
    Metrics_service.encode_export_metrics_service_request
      (Metrics_service.default_export_metrics_service_request
         ~resource_metrics ())
      encoder;
    begin match
        C.send ~path:"/v1/metrics" ~decode:(fun _ -> ())
          (Pbrt.Encoder.to_string encoder)
      with
      | Ok () -> ()
      | Error err -> report_err_ err
    end;
    (* signal completion *)
    List.iter (fun (_,over) -> over()) l;
  in

  let emit_traces (module C: CURL) (l:(Trace.resource_spans list * over_cb) list) =
    Pbrt.Encoder.reset encoder;
    let resource_spans =
      List.fold_left (fun acc (l,_) -> List.rev_append l acc) [] l in
    Trace_service.encode_export_trace_service_request
      (Trace_service.default_export_trace_service_request ~resource_spans ())
      encoder;
    begin match
        C.send ~path:"/v1/traces" ~decode:(fun _ -> ())
          (Pbrt.Encoder.to_string encoder)
      with
      | Ok () -> ()
      | Error err -> report_err_ err
    end;
    (* signal completion *)
    List.iter (fun (_,over) -> over()) l;
  in

  if config.thread then (
    begin
      let m = Mutex.create() in
      set_mutex ~lock:(fun () -> Mutex.lock m) ~unlock:(fun () -> Mutex.unlock m);
    end;

    let module C = Curl() in

    let m = Mutex.create() in
    let cond = Condition.create() in
    let last_wakeup = ref (Mtime_clock.now()) in

    (* TODO: move this into config *)
    let batch_timeout() : bool =
      let elapsed = Mtime.span (Mtime_clock.now()) !last_wakeup in
      Mtime.Span.compare elapsed Mtime.Span.(200 * ms) >= 0
    in

    let emit_metrics ?(force=false) () : bool =
      if (force && not (E_metrics.is_empty())) ||
          (not force && E_metrics.is_big_enough ()) then (
        let batch = ref [] in
        E_metrics.pop_iter_all (fun l -> batch := l :: !batch);
        emit_metrics (module C) !batch;
        true
      ) else false
    in
    let emit_traces ?(force=false) () : bool =
      if (force && not (E_trace.is_empty())) ||
          (not force && E_trace.is_big_enough ()) then (
        let batch = ref [] in
        E_trace.pop_iter_all (fun l -> batch := l :: !batch);
        emit_traces (module C) !batch;
        true
      ) else false
    in

    let bg_thread () =
      while !continue do
        if emit_metrics () then ()
        else if emit_traces () then ()
        else (
          (* wait *)
          let@ () = with_mutex_ m in
          Condition.wait cond m;
        )
      done;
      (* flush remaining events *)
      ignore (emit_traces ~force:true () : bool);
      ignore (emit_metrics ~force:true () : bool);
      C.cleanup();
    in

    let _: Thread.t = Thread.create bg_thread () in

    let wakeup () =
      last_wakeup := Mtime_clock.now();
      with_mutex_ m (fun () -> Condition.signal cond)
    in

    (* wake up if a batch is full *)
    on_metrics_full wakeup;
    on_trace_full wakeup;

    let module M = struct
      let push_trace e ~over =
        E_trace.push (e,over);
        if batch_timeout() then wakeup()
      let push_metrics e ~over =
        E_metrics.push (e,over);
        if batch_timeout() then wakeup()
      let cleanup () =
        continue := false;
        with_mutex_ m (fun () -> Condition.broadcast cond)
    end in
    (module M)
  ) else (
    assert false
  )

module Backend(Arg : sig val config : Config.t end)()
  : Opentelemetry.Collector.BACKEND
= struct
  include Gen_ids()

  include (val mk_emitter ~config:Arg.config ())

  open Opentelemetry.Proto
  open Opentelemetry.Collector

  let send_trace : Trace.resource_spans list sender = {
    send=fun l ~over ~ret ->
      let@() = with_lock_ in
      if !debug_ then Format.eprintf "send spans %a@." (Format.pp_print_list Trace.pp_resource_spans) l;
      push_trace l ~over;
      ret()
  }

  let send_metrics : Metrics.resource_metrics list sender = {
    send=fun m ~over ~ret ->
      let@() = with_lock_ in
      if !debug_ then Format.eprintf "send metrics %a@." (Format.pp_print_list Metrics.pp_resource_metrics) m;
      push_metrics m ~over;
      ret()
  }
end

let setup_ ~(config:Config.t) () =
  debug_ := config.debug;
  let module B = Backend(struct let config=config end)() in
  Opentelemetry.Collector.backend := Some (module B);
  B.cleanup

let setup ?(config=Config.make()) () =
  let cleanup = setup_ ~config () in
  at_exit cleanup

let with_setup ?(config=Config.make()) f =
  let cleanup = setup_ ~config () in
  Fun.protect ~finally:cleanup f
