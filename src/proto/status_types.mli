(** status.proto Types *)



(** {2 Types} *)

type status = {
  code : int32;
  message : bytes;
  details : bytes list;
}


(** {2 Default values} *)

val default_status : 
  ?code:int32 ->
  ?message:bytes ->
  ?details:bytes list ->
  unit ->
  status
(** [default_status ()] is the default value for type [status] *)
