(** status.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_status : Format.formatter -> Status_types.status -> unit 
(** [pp_status v] formats v *)
