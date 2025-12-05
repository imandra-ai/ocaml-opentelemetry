(** Consumer that accepts items from a bounded queue *)

type 'a t = {
  active: unit -> bool;  (** Still running? Must be fast and thread-safe *)
  tick: unit -> unit;
      (** Regularly called, eg to emit metrics, check timeouts, etc. Must be
          thread safe. *)
  shutdown: on_done:(unit -> unit) -> unit;
      (** Shutdown the consumer as soon as possible, call [on_done()] once it's
          done. *)
}
(** A consumer for signals of type ['a] *)

type 'a consumer = 'a t

let[@inline] active (self : _ t) = self.active ()

let[@inline] shutdown (self : _ t) ~on_done = self.shutdown ~on_done

module Builder = struct
  type 'a t = { start_consuming: 'a Bounded_queue.t -> 'a consumer }
  (** A builder that will create a consumer for a given queue, start the
      consumer so it starts consuming from the queue, and return the consumer.
  *)

  let start_consuming (self : _ t) bq = self.start_consuming bq
end

type any_resource_builder = Any_resource.t Builder.t
(** The type that's useful for OTEL backends *)
