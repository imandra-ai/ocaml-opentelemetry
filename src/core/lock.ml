let lock_ : (unit -> unit) ref = ref ignore

let unlock_ : (unit -> unit) ref = ref ignore

let set_mutex ~lock ~unlock : unit =
  lock_ := lock;
  unlock_ := unlock

let[@inline] with_lock f =
  !lock_ ();
  match f () with
  | x ->
    !unlock_ ();
    x
  | exception e ->
    !unlock_ ();
    Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
