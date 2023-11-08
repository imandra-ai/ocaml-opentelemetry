(** common.proto Types *)



(** {2 Types} *)

type any_value =
  | Bytes_value of bytes
  | Kvlist_value of key_value_list
  | Array_value of array_value
  | Double_value of float
  | Int_value of int64
  | Bool_value of bool
  | String_value of string

and key_value_list = {
  values : key_value list;
}

and key_value = {
  key : string;
  value : any_value option;
}

and array_value = {
  values : any_value list;
}

type instrumentation_scope = {
  name : string;
  version : string;
  attributes : key_value list;
  dropped_attributes_count : int32;
}


(** {2 Default values} *)

val default_any_value : unit -> any_value
(** [default_any_value ()] is the default value for type [any_value] *)

val default_key_value_list : 
  ?values:key_value list ->
  unit ->
  key_value_list
(** [default_key_value_list ()] is the default value for type [key_value_list] *)

val default_key_value : 
  ?key:string ->
  ?value:any_value option ->
  unit ->
  key_value
(** [default_key_value ()] is the default value for type [key_value] *)

val default_array_value : 
  ?values:any_value list ->
  unit ->
  array_value
(** [default_array_value ()] is the default value for type [array_value] *)

val default_instrumentation_scope : 
  ?name:string ->
  ?version:string ->
  ?attributes:key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  instrumentation_scope
(** [default_instrumentation_scope ()] is the default value for type [instrumentation_scope] *)
