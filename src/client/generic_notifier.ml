module type IO = Generic_io.S

module type S = sig
  module IO : IO

  type t

  val create : unit -> t

  val delete : t -> unit

  val trigger : t -> unit

  val wait : t -> unit IO.t

  val register_bounded_queue : t -> _ Bounded_queue.Recv.t -> unit
end
