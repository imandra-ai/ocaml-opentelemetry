module UM = Util_mutex

type 'a t = {
  mutex: Mutex.t;
  cond: Condition.t;
  q: 'a Queue.t;
  mutable closed: bool;
}

exception Closed

let create () : _ t =
  {
    mutex = Mutex.create ();
    cond = Condition.create ();
    q = Queue.create ();
    closed = false;
  }

let close (self : _ t) =
  UM.protect self.mutex @@ fun () ->
  if not self.closed then (
    self.closed <- true;
    Condition.broadcast self.cond (* awake waiters so they fail  *)
  )

let push (self : _ t) x : unit =
  UM.protect self.mutex @@ fun () ->
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
  UM.protect self.mutex loop

let pop_all (self : 'a t) into : unit =
  let rec loop () =
    if Queue.is_empty self.q then (
      if self.closed then raise Closed;
      Condition.wait self.cond self.mutex;
      (loop [@tailcall]) ()
    ) else
      Queue.transfer self.q into
  in
  UM.protect self.mutex loop
