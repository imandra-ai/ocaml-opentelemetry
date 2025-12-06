(** Generic IO *)
module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val protect : finally:(unit -> unit t) -> (unit -> 'a t) -> 'a t
end

module type S_WITH_CONCURRENCY = sig
  include S

  val sleep_s : float -> unit t

  val spawn : (unit -> unit t) -> unit
end

module Direct_style : S with type 'a t = 'a = struct
  type 'a t = 'a

  let[@inline] return x = x

  let[@inline] ( let* ) x f = f x

  let protect = Fun.protect
end
