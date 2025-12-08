module Atomic = Opentelemetry_atomic.Atomic

(** Update loop *)
let update_cas (type res) (self : 'a Atomic.t) (f : 'a -> res * 'a) : res =
  let exception Ret of res in
  let backoff = ref 1 in
  try
    while true do
      let old_val = Atomic.get self in
      let res, new_val = f old_val in
      if Atomic.compare_and_set self old_val new_val then
        raise_notrace (Ret res);

      Opentelemetry_domain.relax_loop !backoff;
      backoff := min 128 (2 * !backoff)
    done
  with Ret r -> r
