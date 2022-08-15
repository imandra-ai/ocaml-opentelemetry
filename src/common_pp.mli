(** common.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_any_value : Format.formatter -> Common_types.any_value -> unit 
(** [pp_any_value v] formats v *)

val pp_array_value : Format.formatter -> Common_types.array_value -> unit 
(** [pp_array_value v] formats v *)

val pp_key_value_list : Format.formatter -> Common_types.key_value_list -> unit 
(** [pp_key_value_list v] formats v *)

val pp_key_value : Format.formatter -> Common_types.key_value -> unit 
(** [pp_key_value v] formats v *)

val pp_instrumentation_scope : Format.formatter -> Common_types.instrumentation_scope -> unit 
(** [pp_instrumentation_scope v] formats v *)
