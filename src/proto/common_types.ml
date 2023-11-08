[@@@ocaml.warning "-27-30-39"]


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

let rec default_any_value () : any_value = Bytes_value (Bytes.create 0)

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

and default_array_value 
  ?values:((values:any_value list) = [])
  () : array_value  = {
  values;
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
