type cb = unit -> unit

type t = { cbs: cb Alist.t } [@@unboxed]

let create () : t = { cbs = Alist.make () }

let[@inline] register self f = Alist.add self.cbs f

let[@inline] trigger self = List.iter (fun f -> f ()) (Alist.get self.cbs)
