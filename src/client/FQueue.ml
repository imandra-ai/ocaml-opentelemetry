type 'a t = {
  arr: 'a array;
  mutable i: int;
}

let create ~dummy n : _ t =
  assert (n >= 1);
  { arr = Array.make n dummy; i = 0 }

let[@inline] size self = self.i

let[@inline] is_full self = self.i = Array.length self.arr

let push (self : _ t) x : bool =
  if is_full self then
    false
  else (
    self.arr.(self.i) <- x;
    self.i <- 1 + self.i;
    true
  )

let pop_iter_all (self : _ t) f =
  for j = 0 to self.i - 1 do
    f self.arr.(j)
  done;
  self.i <- 0
