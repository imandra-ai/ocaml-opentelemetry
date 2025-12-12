(** Storage using Lwt keys *)

open Opentelemetry_ambient_context_core

open struct
  let lwt_context_key : Context.t Lwt.key = Lwt.new_key ()
end

let storage : Storage.t =
  {
    name = "lwt";
    get_context =
      (fun () -> Lwt.get lwt_context_key |> Option.value ~default:Hmap.empty);
    with_context = (fun ctx f -> Lwt.with_value lwt_context_key (Some ctx) f);
  }
