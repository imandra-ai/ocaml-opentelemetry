(** Notification that can be used on the consumer side of a bounded queue *)

type t = {
  notified: bool Atomic.t;
  cond: unit Lwt_condition.t;
  notification: int;
  deleted: bool Atomic.t;
}

let create () : t =
  let notified = Atomic.make false in
  let cond = Lwt_condition.create () in
  let notification =
    Lwt_unix.make_notification (fun () ->
        Atomic.set notified false;
        Lwt_condition.broadcast cond ())
  in
  { notified; notification; cond; deleted = Atomic.make false }

let delete self : unit =
  if not (Atomic.exchange self.deleted true) then
    Lwt_unix.stop_notification self.notification

let trigger (self : t) : unit =
  if not (Atomic.exchange self.notified true) then
    Lwt_unix.send_notification self.notification

let wait (self : t) : unit Lwt.t = Lwt_condition.wait self.cond

let register_bounded_queue (self : t) (q : _ Bounded_queue.t) : unit =
  Bounded_queue.on_non_empty q (fun () -> trigger self)
