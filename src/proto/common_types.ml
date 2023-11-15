[@@@ocaml.warning "-27-30-39"]


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

type instrumentation_scope = {
  name : string;
  version : string;
  attributes : key_value list;
  dropped_attributes_count : int32;
}

let rec default_any_value () : any_value = String_value ("")

and default_array_value 
  ?values:((values:any_value list) = [])
  () : array_value  = {
  values;
}

and default_key_value_list 
  ?values:((values:key_value list) = [])
  () : key_value_list  = {
  values;
}

and default_key_value 
  ?key:((key:string) = "")
  ?value:((value:any_value option) = None)
  () : key_value  = {
  key;
  value;
}

let rec default_instrumentation_scope 
  ?name:((name:string) = "")
  ?version:((version:string) = "")
  ?attributes:((attributes:key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : instrumentation_scope  = {
  name;
  version;
  attributes;
  dropped_attributes_count;
}
