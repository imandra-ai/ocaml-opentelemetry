type error = Export_error.t

(* TODO: emit this in a metric in [tick()] if self tracing is enabled? *)

(** Number of errors met during export *)
let n_errors = Atomic.make 0

(* TODO: put this somewhere with an interval limiter to 30s

    (* there is a possible race condition here, as several threads might update
       metrics at the same time. But that's harmless. *)
    if add_own_metrics then (
      Atomic.set last_sent_metrics now;
      let open OT.Metrics in
      [
        make_resource_metrics
          [
            sum ~name:"otel.export.dropped" ~is_monotonic:true
              [
                int
                  ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                  ~now:(Mtime.to_uint64_ns now) (Atomic.get n_dropped);
              ];
            sum ~name:"otel.export.errors" ~is_monotonic:true
              [
                int
                  ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                  ~now:(Mtime.to_uint64_ns now) (Atomic.get n_errors);
              ];
          ];
      ]
    ) else
      []
*)

module type IO = Generic_io.S_WITH_CONCURRENCY

module type HTTPC = sig
  module IO : IO

  type t

  val create : unit -> t

  val send :
    t ->
    url:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result IO.t

  val cleanup : t -> unit
end

module Make
    (IO : IO)
    (Notifier : Generic_notifier.S with type 'a IO.t = 'a IO.t)
    (Httpc : HTTPC with type 'a IO.t = 'a IO.t) : sig
  val consumer :
    ?override_n_workers:int ->
    ticker_task:float option ->
    stop:bool Atomic.t ->
    config:Client_config.t ->
    unit ->
    Consumer.any_resource_builder
  (** Create a consumer.
      @param stop
        shared stop variable, set to true to stop this (and maybe other tasks)
      @param ticker_task
        controls whether we start a task to call [tick] at the given interval in
        seconds, or [None] to not start such a task at all. *)
end = struct
  module Proto = Opentelemetry_proto
  open IO

  type other_config = {
    override_n_workers: int option;
    ticker_task: float option;
  }

  type state = {
    stop: bool Atomic.t;
    cleaned: bool Atomic.t;  (** True when we cleaned up after closing *)
    config: Client_config.t;
    other_config: other_config;
    q: Any_resource.t Bounded_queue.t;
    notify: Notifier.t;
  }

  let shutdown self =
    Atomic.set self.stop true;
    if not (Atomic.exchange self.cleaned true) then (
      Notifier.trigger self.notify;
      Notifier.delete self.notify
    )

  let send_http_ (self : state) (httpc : Httpc.t) ~backoff ~url (data : string)
      : unit IO.t =
    let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
    match r with
    | Ok () ->
      Util_backoff.on_success backoff;
      IO.return ()
    | Error `Sysbreak ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      Atomic.set self.stop true;
      IO.return ()
    | Error err ->
      Atomic.incr n_errors;
      Export_error.report_err err;
      (* avoid crazy error loop *)
      let dur_s = Util_backoff.cur_duration_s backoff in
      Util_backoff.on_error backoff;
      IO.sleep_s (dur_s +. Random.float (dur_s /. 10.))

  let send_metrics_http (st : state) client ~encoder ~backoff
      (l : Proto.Metrics.resource_metrics list) =
    let msg = Signal.Encode.metrics ~encoder l in
    send_http_ st client msg ~backoff ~url:st.config.url_metrics

  let send_traces_http st client ~encoder ~backoff
      (l : Proto.Trace.resource_spans list) =
    let msg = Signal.Encode.traces ~encoder l in
    send_http_ st client msg ~backoff ~url:st.config.url_traces

  let send_logs_http st client ~encoder ~backoff
      (l : Proto.Logs.resource_logs list) =
    let msg = Signal.Encode.logs ~encoder l in
    send_http_ st client msg ~backoff ~url:st.config.url_logs

  let tick (self : state) = Notifier.trigger self.notify

  let start_worker (self : state) : unit =
    let client = Httpc.create () in
    let encoder = Pbrt.Encoder.create () in
    let backoff = Util_backoff.create () in

    (* loop on [q] *)
    let rec loop () : unit IO.t =
      if Atomic.get self.stop then
        IO.return ()
      else
        let* () =
          match Bounded_queue.try_pop self.q with
          | `Closed ->
            shutdown self;
            IO.return ()
          | `Empty -> Notifier.wait self.notify
          | `Item (R_logs logs) ->
            send_logs_http self client ~encoder ~backoff logs
          | `Item (R_metrics ms) ->
            send_metrics_http self client ~encoder ~backoff ms
          | `Item (R_spans spans) ->
            send_traces_http self client ~encoder ~backoff spans
        in
        loop ()
    in

    IO.spawn (fun () ->
        IO.protect loop ~finally:(fun () ->
            Httpc.cleanup client;
            IO.return ()))

  let start_ticker (self : state) ~(interval_s : float) : unit =
    let rec loop () : unit IO.t =
      if Atomic.get self.stop then
        IO.return ()
      else
        let* () = IO.sleep_s interval_s in
        tick self;
        loop ()
    in
    IO.spawn loop

  let default_n_workers = 50

  let create_state ?override_n_workers ~ticker_task ~stop ~config ~q () : state
      =
    let other_config = { override_n_workers; ticker_task } in
    let self =
      {
        stop;
        config;
        other_config;
        q;
        cleaned = Atomic.make false;
        notify = Notifier.create ();
      }
    in

    (* start workers *)
    let n_workers =
      min 2
        (max 500
           (match
              ( self.other_config.override_n_workers,
                self.config.http_concurrency_level )
            with
           | Some n, _ -> n
           | None, Some n -> n
           | None, None -> default_n_workers))
    in

    for _i = 1 to n_workers do
      start_worker self
    done;

    (* start ticker *)
    (match self.other_config.ticker_task with
    | None -> ()
    | Some interval_s -> start_ticker self ~interval_s);

    self

  let to_consumer (self : state) : Any_resource.t Consumer.t =
    let active () = not (Atomic.get self.stop) in
    let shutdown ~on_done =
      shutdown self;
      on_done ()
    in
    let tick () = tick self in
    { active; tick; shutdown }

  let consumer ?override_n_workers ~ticker_task ~stop ~config () :
      Consumer.any_resource_builder =
    {
      start_consuming =
        (fun q ->
          let st =
            create_state ?override_n_workers ~ticker_task ~stop ~config ~q ()
          in
          to_consumer st);
    }
end
