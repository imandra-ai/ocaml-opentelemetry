open Common_

type error = Export_error.t

(** Number of errors met during export *)
let n_errors = Atomic.make 0

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
    config:Client_config.t ->
    unit ->
    Consumer.any_resource_builder
  (** Make a consumer builder, ie. a builder function that will take a bounded
      queue of signals, and start a consumer to process these signals and send
      them somewhere using HTTP.
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

  type status =
    | Active
    | Shutting_down
    | Stopped

  type state = {
    active: Aswitch.t;  (** Public facing switch *)
    active_trigger: Aswitch.trigger;
    status: status Atomic.t;
        (** Internal status, including the shutting down process *)
    config: Client_config.t;
    other_config: other_config;
    q: Any_resource.t Bounded_queue.t;
    notify: Notifier.t;
    n_workers: int Atomic.t;  (** Current number of workers *)
  }

  let shutdown self : unit =
    let old_status =
      Util_atomic.update_cas self.status @@ fun status ->
      match status with
      | Stopped -> status, status
      | Shutting_down -> status, status
      | Active -> status, Shutting_down
    in

    match old_status with
    | Stopped -> ()
    | Shutting_down ->
      (* last worker to stop will call [on_done] *)
      ()
    | Active ->
      (* notify potentially asleep workers *)
      Notifier.trigger self.notify;
      Notifier.delete self.notify

  let send_http_ (self : state) (httpc : Httpc.t) ~backoff ~url (data : string)
      : unit IO.t =
    let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
    match r with
    | Ok () ->
      Util_net_backoff.on_success backoff;
      IO.return ()
    | Error `Sysbreak ->
      Printf.eprintf "ctrl-c captured, stopping\n%!";
      shutdown self;
      IO.return ()
    | Error err ->
      Atomic.incr n_errors;
      Export_error.report_err err;
      (* avoid crazy error loop *)
      let dur_s = Util_net_backoff.on_error backoff in
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

  let tick (self : state) =
    if Aswitch.is_on self.active then Notifier.trigger self.notify

  (** Shutdown one worker, when the queue is closed *)
  let shutdown_worker (self : state) : unit =
    (* let tid = Thread.id @@ Thread.self () in
       Printf.eprintf "worker %d: shutting down\n%!" tid; *)
    if Atomic.fetch_and_add self.n_workers (-1) = 1 then (
      (* we were the last worker *)
      (* Printf.eprintf "worker %d: last one!\n%!" tid; *)
      Atomic.set self.status Stopped;
      Aswitch.turn_off self.active_trigger
    )

  let start_worker (self : state) : unit =
    let client = Httpc.create () in
    let encoder = Pbrt.Encoder.create () in
    let backoff = Util_net_backoff.create () in

    (* loop on [q] *)
    let rec loop () : unit IO.t =
      (* first look at the queue, to drain it *)
      match Bounded_queue.try_pop self.q with
      | `Closed ->
        (* this worker shuts down, others might still be busy *)
        shutdown_worker self;
        IO.return ()
      | `Item (R_logs logs) ->
        let* () = send_logs_http self client ~encoder ~backoff logs in
        loop ()
      | `Item (R_metrics ms) ->
        let* () = send_metrics_http self client ~encoder ~backoff ms in
        loop ()
      | `Item (R_spans spans) ->
        let* () = send_traces_http self client ~encoder ~backoff spans in
        loop ()
      | `Empty ->
        (* Printf.eprintf "worker %d: empty queue\n%!" tid; *)
        (match Atomic.get self.status with
        | Stopped ->
          assert false
          (* shouldn't happen without us going through [Shutting_down] *)
        | Shutting_down ->
          shutdown_worker self;
          IO.return ()
        | Active ->
          let* () = Notifier.wait self.notify in
          loop ())
    in

    IO.spawn (fun () ->
        IO.protect loop ~finally:(fun () ->
            Httpc.cleanup client;
            IO.return ()))

  let start_ticker (self : state) ~(interval_s : float) : unit =
    let rec loop () : unit IO.t =
      match Atomic.get self.status with
      | Stopped | Shutting_down -> IO.return ()
      | Active ->
        let* () = IO.sleep_s interval_s in
        if Aswitch.is_on self.active then tick self;
        loop ()
    in
    IO.spawn loop

  let default_n_workers = 50

  let create_state ?override_n_workers ~ticker_task ~config ~q () : state =
    let active, active_trigger = Aswitch.create () in
    let other_config = { override_n_workers; ticker_task } in
    let self =
      {
        active;
        active_trigger;
        status = Atomic.make Active;
        config;
        other_config;
        q;
        notify = Notifier.create ();
        n_workers = Atomic.make 0;
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

    ignore (Atomic.fetch_and_add self.n_workers n_workers : int);
    for _i = 1 to n_workers do
      start_worker self
    done;

    Notifier.register_bounded_queue self.notify q;

    (* start ticker *)
    (match self.other_config.ticker_task with
    | None -> ()
    | Some interval_s -> start_ticker self ~interval_s);

    self

  let self_metrics (self : state) : OTEL.Metrics.t list =
    let open OTEL.Metrics in
    let now = Mtime_clock.now () in
    [
      sum ~name:"otel-ocaml.export.discarded-by-bounded-queue"
        ~is_monotonic:true
        [
          int ~now:(Mtime.to_uint64_ns now) (Bounded_queue.num_discarded self.q);
        ];
      sum ~name:"otel-ocaml.export.errors" ~is_monotonic:true
        [ int ~now:(Mtime.to_uint64_ns now) (Atomic.get n_errors) ];
    ]

  let to_consumer (self : state) : Any_resource.t Consumer.t =
    let shutdown () = shutdown self in
    let tick () = tick self in
    let self_metrics () = self_metrics self in
    { active = (fun () -> self.active); tick; shutdown; self_metrics }

  let consumer ?override_n_workers ~ticker_task ~config () :
      Consumer.any_resource_builder =
    {
      start_consuming =
        (fun q ->
          let st =
            create_state ?override_n_workers ~ticker_task ~config ~q ()
          in
          to_consumer st);
    }
end
