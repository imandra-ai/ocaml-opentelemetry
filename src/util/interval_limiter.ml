type t = {
  min_interval: Mtime.span;
  last: Mtime.t Atomic.t;
}

let create ~min_interval () : t =
  { min_interval; last = Atomic.make Mtime.min_stamp }

let make_attempt (self : t) : bool =
  let now = Mtime_clock.now () in
  let last = Atomic.get self.last in
  let elapsed = Mtime.span last now in
  if Mtime.Span.compare elapsed self.min_interval >= 0 then
    (* attempts succeeds, unless another thread updated [self.last]
       in the mean time *)
    Atomic.compare_and_set self.last last now
  else
    false
