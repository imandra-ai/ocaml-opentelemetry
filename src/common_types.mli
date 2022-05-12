(** common.proto Types *)



(** {2 Types} *)

type any_value =
  | String_value of string
  | Bool_value of bool
  | Int_value of int64
  | Double_value of float
  | Array_value of array_value
  | Kvlist_value of key_value_list
  | Bytes_value of bytes

and array_value = {
  values : any_value list;
}

and key_value_list = {
  values : key_value list;
}

and key_value = {
  key : string;
  value : any_value option;
}

type instrumentation_library = {
  name : string;
  version : string;
}


(** {2 Default values} *)

val default_any_value : unit -> any_value
(** [default_any_value ()] is the default value for type [any_value] *)

val default_array_value : 
  ?values:any_value list ->
  unit ->
  array_value
(** [default_array_value ()] is the default value for type [array_value] *)

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

val default_instrumentation_library : 
  ?name:string ->
  ?version:string ->
  unit ->
  instrumentation_library
(** [default_instrumentation_library ()] is the default value for type [instrumentation_library] *)
