(** Storage using Lwt keys *)

open Opentelemetry_ambient_context_core

open struct
  module FLS = Picos.Fiber.FLS

  let fls_context_key : Context.t FLS.t = FLS.create ()

  let get_context () =
    try FLS.get_exn (Picos.Fiber.current ()) fls_context_key
    with _ -> Hmap.empty

  let with_context ctx f =
    match Picos.Fiber.current () with
    | exception _ ->
      (* if run outside a fiber, do nothing *)
      f ()
    | fiber ->
      let old =
        try FLS.get_exn fiber fls_context_key with FLS.Not_set -> Hmap.empty
      in
      FLS.set fiber fls_context_key ctx;
      (match f () with
      | res ->
        FLS.set fiber fls_context_key old;
        res
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        FLS.set fiber fls_context_key old;
        Printexc.raise_with_backtrace exn bt)
end

let storage : Storage.t = { name = "picos_fls"; get_context; with_context }
