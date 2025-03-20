module P_tbl = Picos_aux_htbl

type 'a t = (int64, 'a) P_tbl.t

let create () : _ t =
  P_tbl.create
    ~hashed_type:
      (module struct
        include Int64

        let hash = Hashtbl.hash
      end)
    ()

let find (self : 'a t) (k : int64) : 'a option =
  try Some (P_tbl.find_exn self k) with Not_found -> None

let add (self : _ t) k v : unit =
  let _ok = P_tbl.try_add self k v in
  ignore _ok;
  ()

let remove (self : _ t) k : unit =
  ignore (P_tbl.try_remove self k : bool);
  ()
