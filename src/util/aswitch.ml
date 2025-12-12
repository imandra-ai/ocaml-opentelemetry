open Opentelemetry_atomic

module Int_map = Map.Make (struct
  type t = int

  let compare = compare
end)

type cb = unit -> unit

type state =
  | On of {
      n: int;
      m: cb Int_map.t;  (** removable callbacks *)
      l: cb list;
    }
  | Off

type t = { st: state Atomic.t } [@@unboxed]

type trigger = t

let dummy : t = { st = Atomic.make Off }

let on_turn_off (self : t) (f : cb) : unit =
  let must_fire =
    Util_atomic.update_cas self.st @@ function
    | Off -> true, Off
    | On r -> false, On { r with l = f :: r.l }
  in
  if must_fire then (* call now *) f ()

let turn_off' self =
  (* When calling turn_off' from a signal handler, Trace.message may cause the thread
     to be killed. For this reason, we provide a way to disable tracing here. *)
  match Atomic.exchange self.st Off with
  | Off -> `Was_off
  | On { l; m; n = _ } ->
    List.iter (fun f -> f ()) l;
    Int_map.iter (fun _ f -> f ()) m;
    `Was_on

let[@inline] turn_off self = ignore (turn_off' self : [> `Was_on ])

let[@inline] link parent tr : unit = on_turn_off parent (fun () -> turn_off tr)

let create ?parent () : t * trigger =
  let self = { st = Atomic.make (On { l = []; n = 0; m = Int_map.empty }) } in
  (* if there's a parent, turning the parent off must turn us off too *)
  Option.iter (fun p -> link p self) parent;
  self, self

let[@inline] is_on self : bool =
  match Atomic.get self.st with
  | On _ -> true
  | Off -> false

let[@inline] is_off self = not (is_on self)

let show self = Printf.sprintf "<switch on=%B>" (is_on self)

let pp out self = Format.fprintf out "<switch on=%B>" (is_on self)

module Unsafe = struct
  let trigger_of_switch = Fun.id
end
