open Common_

(** start a thread in the background, running [f()], blocking signals *)
let start_bg_thread (f : unit -> unit) : Thread.t =
  let unix_run () =
    let signals =
      [
        Sys.sigusr1;
        Sys.sigusr2;
        Sys.sigterm;
        Sys.sigpipe;
        Sys.sigalrm;
        Sys.sigstop;
      ]
    in
    ignore (Thread.sigmask Unix.SIG_BLOCK signals : _ list);
    f ()
  in
  (* no signals on Windows *)
  let run () =
    if Sys.win32 then
      f ()
    else
      unix_run ()
  in
  Thread.create run ()

(** thread that calls [tick()] regularly, to help enforce timeouts *)
let setup_ticker_thread ~stop ~sleep_ms (exp : OTEL.Exporter.t) () =
  let sleep_s = float sleep_ms /. 1000. in
  let tick_loop () =
    try
      while not @@ Atomic.get stop do
        Thread.delay sleep_s;
        OTEL.Exporter.tick exp
      done
    with
    | Sync_queue.Closed -> ()
    | exn ->
      (* print and ignore *)
      Printf.eprintf "otel-ocurl: ticker thread: uncaught exn:\n%s\n%!"
        (Printexc.to_string exn)
  in
  start_bg_thread tick_loop

module MCond = struct
  type t = {
    mutex: Mutex.t;
    cond: Condition.t;
  }

  let create () : t = { mutex = Mutex.create (); cond = Condition.create () }

  let signal self = Condition.signal self.cond

  let[@inline] protect self f = Util_mutex.protect self.mutex f

  (** NOTE: the mutex must be acquired *)
  let wait self = Condition.wait self.cond self.mutex

  (** Ensure we get signalled when the queue goes from empty to non-empty *)
  let wakeup_from_bq (self : t) (bq : _ Bounded_queue.t) : unit =
    Bounded_queue.on_non_empty bq (fun () -> signal self)
end
