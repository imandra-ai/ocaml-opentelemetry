(** resource.proto Types *)



(** {2 Types} *)

type resource = {
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
}


(** {2 Default values} *)

val default_resource : 
  ?attributes:Common_types.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  resource
(** [default_resource ()] is the default value for type [resource] *)
