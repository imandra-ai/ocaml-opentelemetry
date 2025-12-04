type t = {
  proba_accept: float;
  n_seen: int Atomic.t;
  n_accepted: int Atomic.t;
}

let create ~proba_accept () : t =
  (* FIXME: either czzry a random state and protect it, or make sure
     we Random.self_init() in the current domain?? *)
  if proba_accept < 0. || proba_accept > 1. then
    invalid_arg "sampler: proba_accept must be in [0., 1.]";
  { proba_accept; n_seen = Atomic.make 0; n_accepted = Atomic.make 0 }

let[@inline] proba_accept self = self.proba_accept

let actual_rate (self : t) : float =
  let accept = Atomic.get self.n_accepted in
  let total = Atomic.get self.n_seen in

  if total = 0 then
    1.
  else
    float accept /. float total

let accept (self : t) : bool =
  Atomic.incr self.n_seen;

  let n = Random.float 1. in
  let res = n < self.proba_accept in

  if res then Atomic.incr self.n_accepted;
  res

open Opentelemetry_emitter

let wrap_emitter (self : t) (e : _ Emitter.t) : _ Emitter.t =
  let enabled () = e.enabled () in
  let closed () = Emitter.closed e in
  let flush_and_close () = Emitter.flush_and_close e in
  let tick ~now = Emitter.tick e ~now in

  let emit l =
    if l <> [] && e.enabled () then (
      let accepted = List.filter (fun _x -> accept self) l in
      if accepted <> [] then Emitter.emit e accepted
    )
  in

  { Emitter.closed; enabled; flush_and_close; tick; emit }
