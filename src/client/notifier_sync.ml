module IO = Generic_io.Direct_style

type t = {
  mutex: Mutex.t;
  cond: Condition.t;
}

let create () : t = { mutex = Mutex.create (); cond = Condition.create () }

let trigger self = Condition.signal self.cond

let delete = ignore

let wait self =
  Mutex.lock self.mutex;
  Condition.wait self.cond self.mutex;
  Mutex.unlock self.mutex

(** Ensure we get signalled when the queue goes from empty to non-empty *)
let register_bounded_queue (self : t) (bq : _ Bounded_queue.t) : unit =
  Bounded_queue.on_non_empty bq (fun () -> trigger self)
