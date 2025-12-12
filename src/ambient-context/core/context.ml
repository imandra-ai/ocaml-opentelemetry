type t = Hmap.t

type 'a key = 'a Hmap.key

let empty : t = Hmap.empty

let[@inline] new_key () : _ key = Hmap.Key.create ()
