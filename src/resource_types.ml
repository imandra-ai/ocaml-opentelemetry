[@@@ocaml.warning "-27-30-39"]


type resource = {
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
}

let rec default_resource 
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : resource  = {
  attributes;
  dropped_attributes_count;
}
