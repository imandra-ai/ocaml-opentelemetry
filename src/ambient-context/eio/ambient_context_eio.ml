module TLS = Ambient_context_thread_local.Thread_local
module Hmap = Ambient_context.Hmap
module Fiber = Eio.Fiber

let _internal_key : Hmap.t Fiber.key = Fiber.create_key ()
let ( let* ) = Option.bind

module M = struct
  let name = "Storage_eio"
  let[@inline] get_map () = Fiber.get _internal_key
  let[@inline] with_map m cb = Fiber.with_binding _internal_key m cb
  let create_key = Hmap.Key.create

  let get k =
     let* context = get_map () in
     Hmap.find k context


  let with_binding k v cb =
     let new_context =
        match get_map () with
        | None -> Hmap.singleton k v
        | Some old_context -> Hmap.add k v old_context
     in
     with_map new_context cb


  let without_binding k cb =
     let new_context =
        match get_map () with
        | None -> Hmap.empty
        | Some old_context -> Hmap.rem k old_context
     in
     with_map new_context cb
end

let storage () : Ambient_context.storage = (module M)
