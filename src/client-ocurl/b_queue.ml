type 'a t = {
  mutex: Mutex.t;
  cond: Condition.t;
  q: 'a Queue.t;
  mutable closed: bool;
}

exception Closed

(* Mutex.protect was added in OCaml 5.1, but we want support back to 4.08 *)
(* cannot inline, otherwise flambda might move code around. (as per Stdlib) *)
let[@inline never] protect m f =
  Mutex.lock m;
  match f () with
  | x ->
    Mutex.unlock m;
    x
  | exception e ->
    (* NOTE: [unlock] does not poll for asynchronous exceptions *)
    Mutex.unlock m;
    Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())

let create () : _ t =
  {
    mutex = Mutex.create ();
    cond = Condition.create ();
    q = Queue.create ();
    closed = false;
  }

let close (self : _ t) =
  protect self.mutex @@ fun () ->
  if not self.closed then (
    self.closed <- true;
    Condition.broadcast self.cond (* awake waiters so they fail  *)
  )

let push (self : _ t) x : unit =
  protect self.mutex @@ fun () ->
  if self.closed then
    raise Closed
  else (
    Queue.push x self.q;
    Condition.signal self.cond
  )

let pop (self : 'a t) : 'a =
  let rec loop () =
    if self.closed then
      raise Closed
    else if Queue.is_empty self.q then (
      Condition.wait self.cond self.mutex;
      (loop [@tailcall]) ()
    ) else (
      let x = Queue.pop self.q in
      x
    )
  in
  protect self.mutex loop

let pop_all (self : 'a t) into : unit =
  let rec loop () =
    if Queue.is_empty self.q then (
      if self.closed then raise Closed;
      Condition.wait self.cond self.mutex;
      (loop [@tailcall]) ()
    ) else
      Queue.transfer self.q into
  in
  protect self.mutex loop
