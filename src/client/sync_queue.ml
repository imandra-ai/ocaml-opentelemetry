module UM = Opentelemetry_util.Util_mutex

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

(* NOTE: the race condition here is benign, assuming no tearing of
 a value of type [bool] which OCaml's memory model should guarantee. *)
let[@inline] closed self = self.closed

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

let try_pop (self : 'a t) : 'a option =
  UM.protect self.mutex @@ fun () ->
  if self.closed then raise Closed;
  try Some (Queue.pop self.q) with Queue.Empty -> None

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

let push_while_not_full ~high_watermark (self : 'a t) (xs : 'a list) : int * int
    =
  UM.protect self.mutex @@ fun () ->
  if self.closed then raise Closed;

  let old_size = Queue.length self.q in
  let xs = ref xs in

  let continue = ref true in
  while !continue && Queue.length self.q < high_watermark do
    match !xs with
    | [] -> continue := false
    | x :: tl_xs ->
      xs := tl_xs;
      Queue.push x self.q
  done;

  (* pushed at least one item *)
  if Queue.length self.q <> old_size then Condition.broadcast self.cond;

  let n_discarded = List.length !xs in
  n_discarded, old_size
