type cb = unit -> unit

type t = { cbs: cb AList.t } [@@unboxed]

let create () : t = { cbs = AList.make () }

let[@inline] on_tick self f = AList.add self.cbs f

let[@inline] tick self = List.iter (fun f -> f ()) (AList.get self.cbs)
