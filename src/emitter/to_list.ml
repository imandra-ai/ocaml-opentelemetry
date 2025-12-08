(** Emitter that stores signals into a list, in reverse order (most recent
    signals first). *)
let to_list (l : 'a list ref) : 'a Emitter.t =
  let closed = Atomic.make false in
  {
    enabled = (fun () -> not (Atomic.get closed));
    emit =
      (fun sigs ->
        if Atomic.get closed then raise Emitter.Closed;
        l := List.rev_append sigs !l);
    tick = (fun ~now:_ -> ());
    closed = (fun () -> Atomic.get closed);
    flush_and_close = (fun () -> Atomic.set closed true);
  }
