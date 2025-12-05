(** Consumer that accepts items from a bounded queue *)

(** A consumer for signals of type ['a] *)
class type ['a] t = object
  method register : 'a Bounded_queue.t -> unit

  method active : unit -> bool

  method start : on_done:(unit -> unit) -> unit

  method shutdown : on_done:(unit -> unit) -> unit
end

let register (self : _ #t) q = self#register q

let active (self : _ #t) = self#active ()

let start (self : _ #t) ~on_done = self#start ~on_done

let shutdown (self : _ #t) ~on_done = self#shutdown ~on_done
