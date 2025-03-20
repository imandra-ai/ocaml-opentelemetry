module Tbl = Hashtbl.Make (struct
  include Int64

  let hash = Hashtbl.hash
end)

type 'a t = {
  m: Mutex.t;
  tbl: 'a Tbl.t;
}

let create () : _ t = { m = Mutex.create (); tbl = Tbl.create 16 }

let find (self : _ t) k : _ option =
  Mutex.lock self.m;
  let r = Tbl.find_opt self.tbl k in
  Mutex.unlock self.m;
  r

let add (self : _ t) k v : unit =
  Mutex.lock self.m;
  Tbl.replace self.tbl k v;
  Mutex.unlock self.m

let remove (self : _ t) k : unit =
  Mutex.lock self.m;
  Tbl.remove self.tbl k;
  Mutex.unlock self.m
