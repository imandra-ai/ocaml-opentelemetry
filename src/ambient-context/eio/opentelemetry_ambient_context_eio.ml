open Opentelemetry_ambient_context_core
module Fiber = Eio.Fiber

open struct
  let fiber_context_key : Context.t Fiber.key = Fiber.create_key ()
end

let storage : Storage.t =
  {
    name = "eio";
    get_context =
      (fun () ->
        Fiber.get fiber_context_key |> Option.value ~default:Hmap.empty);
    with_context = (fun ctx f -> Fiber.with_binding fiber_context_key ctx f);
  }
