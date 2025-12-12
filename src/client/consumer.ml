(** Consumer that accepts items from a bounded queue *)

open Common_

type t = {
  active: unit -> Aswitch.t;
  shutdown: unit -> unit;
      (** Shutdown the consumer as soon as possible. [active] will be turned off
          once the consumer is fully shut down. *)
  tick: unit -> unit;
      (** Regularly called, eg to emit metrics, check timeouts, etc. Must be
          thread safe. *)
  self_metrics: unit -> OTEL.Metrics.t list;  (** Self observing metrics *)
}
(** A consumer for signals of type ['a] *)

type consumer = t

let[@inline] active (self : t) : Aswitch.t = self.active ()

let[@inline] shutdown (self : t) : unit = self.shutdown ()

let[@inline] self_metrics self : _ list = self.self_metrics ()

(** [on_stop e f] calls [f()] when [e] stops, or now if it's already stopped *)
let on_stop self f = Aswitch.on_turn_off (self.active ()) f

module Builder = struct
  type 'a t = { start_consuming: 'a Bounded_queue.Recv.t -> consumer }
  (** A builder that will create a consumer for a given queue, start the
      consumer so it starts consuming from the queue, and return the consumer.
  *)

  let start_consuming (self : _ t) bq = self.start_consuming bq

  let map (type a b) (f : a -> b) (self : b t) : a t =
    {
      start_consuming =
        (fun q ->
          let q = Bounded_queue.Recv.map f q in
          self.start_consuming q);
    }
end

type any_signal_l_builder = OTEL.Any_signal_l.t Builder.t

type any_resource_builder = Any_resource.t Builder.t
(** The type that's useful for HTTP backends *)
