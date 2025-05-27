(*
   TODO: more options from
   https://opentelemetry.io/docs/reference/specification/protocol/exporter/
   *)

open Common_

val get_headers : unit -> (string * string) list

val set_headers : (string * string) list -> unit
(** Set http headers that are sent on every http query to the collector. *)

module Config = Config

val create_backend :
  ?stop:bool Atomic.t ->
  ?config:Config.t ->
  Eio_unix.Stdenv.base ->
  (module Opentelemetry.Collector.BACKEND)
(** Create a new backend using lwt and cohttp

    NOTE [after_cleanup] optional parameter removed since NEXT_RELEASE *)

val setup :
  ?stop:bool Atomic.t -> ?config:Config.t -> ?enable:bool -> Eio_unix.Stdenv.base -> unit
(** Setup endpoint. This modifies {!Opentelemetry.Collector.backend}.
    @param enable
      actually setup the backend (default true). This can be used to
      enable/disable the setup depending on CLI arguments or environment.
    @param config configuration to use
    @param stop
      an atomic boolean. When it becomes true, background threads will all stop
      after a little while. *)

val remove_backend : unit -> unit
(** Shutdown current backend
    @since NEXT_RELEASE *)

val with_setup :
  ?stop:bool Atomic.t ->
  ?config:Config.t ->
  ?enable:bool ->
  Eio_unix.Stdenv.base ->
  (unit -> 'a) ->
  'a
(** [with_setup () f] is like [setup(); f()] but takes care of cleaning up after
    [f()] returns See {!setup} for more details. *)
