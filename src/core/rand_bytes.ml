let initialized_ = Atomic.make false

let[@inline never] actually_init () = Random.self_init ()

let[@inline] maybe_init () =
  if not (Atomic.exchange initialized_ true) then actually_init ()

let default_rand_bytes_8 () : bytes =
  maybe_init ();
  let b = Bytes.create 8 in
  for i = 0 to 1 do
    (* rely on the stdlib's [Random] being thread-or-domain safe *)
    let r = Random.bits () in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.bits () in
  Bytes.set b 6 (Char.chr (r land 0xff));
  Bytes.set b 7 (Char.chr ((r lsr 8) land 0xff));
  b

let default_rand_bytes_16 () : bytes =
  maybe_init ();
  let b = Bytes.create 16 in
  for i = 0 to 4 do
    (* rely on the stdlib's [Random] being thread-or-domain safe *)
    let r = Random.bits () in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.bits () in
  Bytes.set b 15 (Char.chr (r land 0xff));
  (* last byte *)
  b

let rand_bytes_16_ref = ref default_rand_bytes_16

let rand_bytes_8_ref = ref default_rand_bytes_8

(** Generate a 16B identifier *)
let[@inline] rand_bytes_16 () = !rand_bytes_16_ref ()

(** Generate an 8B identifier *)
let[@inline] rand_bytes_8 () = !rand_bytes_8_ref ()
