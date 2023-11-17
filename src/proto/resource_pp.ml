[@@@ocaml.warning "-27-30-39"]

let rec pp_resource fmt (v:Resource_types.resource) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Resource_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Resource_types.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
