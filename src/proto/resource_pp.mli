(** resource.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_resource : Format.formatter -> Resource_types.resource -> unit 
(** [pp_resource v] formats v *)
