module Dom = Opentelemetry_domain
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
    (* backoff *)
    Dom.relax_loop !backoff;
    backoff := min 128 (2 * !backoff)
  done

let pop_all (type res) self : res list =
  let exception Return of res list in
  let backoff = ref 1 in
  try
    while true do
      let l = Atomic.get self in
      if Atomic.compare_and_set self l [] then raise_notrace (Return l);

      (* backoff *)
      Dom.relax_loop !backoff;
      backoff := min 128 (2 * !backoff)
    done
  with Return r -> r
