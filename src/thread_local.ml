module A = Opentelemetry_atomic.Atomic

type key = int

let get_key_ () : key = Thread.id (Thread.self ())

module Key_map_ = Map.Make (struct
  type t = key

  let compare : t -> t -> int = compare
end)

type 'a t = 'a ref Key_map_.t A.t

let create () : _ t = A.make Key_map_.empty

let get_exn (self : _ t) =
  let m = A.get self in
  let key = get_key_ () in
  !(Key_map_.find key m)

let[@inline] get self = try Some (get_exn self) with Not_found -> None

let[@inline] get_or ~default self = try get_exn self with Not_found -> default

(* remove reference for the key *)
let[@inline] remove_ref_ self key : unit =
  while
    let m = A.get self in
    let m' = Key_map_.remove key m in
    not (A.compare_and_set self m m')
  do
    ()
  done

(* get or associate a reference to [key], and return it.
   Also return a function to remove the reference if we just created it. *)
let get_or_create_ref_ (self : _ t) key ~v : _ ref * _ option =
  try
    let r = Key_map_.find key (A.get self) in
    let old = !r in
    r := v;
    r, Some old
  with Not_found ->
    let r = ref v in
    while
      let m = A.get self in
      let m' = Key_map_.add key r m in
      not (A.compare_and_set self m m')
    do
      ()
    done;
    r, None

let with_ self v f =
  let key = get_key_ () in
  let r, old = get_or_create_ref_ self key ~v in

  let restore_ () : unit =
    match old with
    | None -> remove_ref_ self key
    | Some old -> r := old
  in

  try
    let res = f old in
    restore_ ();
    res
  with e ->
    restore_ ();
    raise e
