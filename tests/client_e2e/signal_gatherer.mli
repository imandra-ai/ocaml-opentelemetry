(** A test utility for running a signal emitting executable alongside a minimal
    server that can receive the signals make them available for inspection. *)

val gather_signals :
  ?port:int -> string list -> Opentelemetry_client.Signal.t list
(** [gather_signals program_to_test] is a list of all the signals emitted by the
    [program_to_test], which the server was able to record. This function
    assumes that the program to test will be sending its signals to the
    localhost on [port].

    @param port
      the port where signals will be received. Default is port set in
      {!Opentelemetry_client.Config.default_url}. *)

val run : ?port:int -> unit -> unit
(** [run ()] runs a signal gathering server and prints all batches of signals
    received to stdout.

    @param port
      the port where signals will be received. Default is port set in
      {!Opentelemetry_client.Config.default_url}. *)
