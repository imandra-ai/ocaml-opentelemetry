module Atomic = Opentelemetry_atomic.Atomic

type 'a t = 'a list Atomic.t

let make () = Atomic.make []

let[@inline] is_empty self : bool =
  match Atomic.get self with
  | [] -> true
  | _ :: _ -> false

let get = Atomic.get

let add self x =
  let backoff = ref 1 in
  while
    let old = Atomic.get self in
    let l' = x :: old in
    not (Atomic.compare_and_set self old l')
  do
    Opentelemetry_domain.relax_loop !backoff;
    backoff := min 128 (2 * !backoff)
  done

let rec pop_all self =
  let l = Atomic.get self in
  if Atomic.compare_and_set self l [] then
    l
  else
    pop_all self
