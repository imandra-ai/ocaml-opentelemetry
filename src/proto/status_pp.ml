[@@@ocaml.warning "-27-30-39"]

let rec pp_status fmt (v:Status_types.status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "code" Pbrt.Pp.pp_int32 fmt v.Status_types.code;
    Pbrt.Pp.pp_record_field ~first:false "message" Pbrt.Pp.pp_bytes fmt v.Status_types.message;
    Pbrt.Pp.pp_record_field ~first:false "details" (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes) fmt v.Status_types.details;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
