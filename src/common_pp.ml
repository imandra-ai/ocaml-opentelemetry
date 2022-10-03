[@@@ocaml.warning "-27-30-39"]

let rec pp_any_value fmt (v:Common_types.any_value) =
  match v with
  | Common_types.String_value x -> Format.fprintf fmt "@[<hv2>String_value(@,%a)@]" Pbrt.Pp.pp_string x
  | Common_types.Bool_value x -> Format.fprintf fmt "@[<hv2>Bool_value(@,%a)@]" Pbrt.Pp.pp_bool x
  | Common_types.Int_value x -> Format.fprintf fmt "@[<hv2>Int_value(@,%a)@]" Pbrt.Pp.pp_int64 x
  | Common_types.Double_value x -> Format.fprintf fmt "@[<hv2>Double_value(@,%a)@]" Pbrt.Pp.pp_float x
  | Common_types.Array_value x -> Format.fprintf fmt "@[<hv2>Array_value(@,%a)@]" pp_array_value x
  | Common_types.Kvlist_value x -> Format.fprintf fmt "@[<hv2>Kvlist_value(@,%a)@]" pp_key_value_list x
  | Common_types.Bytes_value x -> Format.fprintf fmt "@[<hv2>Bytes_value(@,%a)@]" Pbrt.Pp.pp_bytes x

and pp_array_value fmt (v:Common_types.array_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_any_value) fmt v.Common_types.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value_list fmt (v:Common_types.key_value_list) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_key_value) fmt v.Common_types.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value fmt (v:Common_types.key_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "key" Pbrt.Pp.pp_string fmt v.Common_types.key;
    Pbrt.Pp.pp_record_field ~first:false "value" (Pbrt.Pp.pp_option pp_any_value) fmt v.Common_types.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_instrumentation_scope fmt (v:Common_types.instrumentation_scope) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Common_types.name;
    Pbrt.Pp.pp_record_field ~first:false "version" Pbrt.Pp.pp_string fmt v.Common_types.version;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list pp_key_value) fmt v.Common_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Common_types.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
