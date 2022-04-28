[@@@ocaml.warning "-27-30-39"]


type status = {
  code : int32;
  message : bytes;
  details : bytes list;
}

let rec default_status 
  ?code:((code:int32) = 0l)
  ?message:((message:bytes) = Bytes.create 0)
  ?details:((details:bytes list) = [])
  () : status  = {
  code;
  message;
  details;
}
