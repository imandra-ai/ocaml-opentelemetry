type t = Client.Config.t
(** Configuration.

    To build one, use {!make} below. This might be extended with more fields in
    the future. *)

val pp : Format.formatter -> t -> unit

val make : (unit -> t) Client.Config.make
(** Make a configuration {!t}. *)

module Env : Client.Config.Env
