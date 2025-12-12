open Opentelemetry_ambient_context_core

open struct
  module TLS = Thread_local_storage

  (* key used to access the context *)
  let tls_k_context : Context.t TLS.t = TLS.create ()
end

let storage : Storage.t =
  {
    name = "tls";
    get_context =
      (fun () -> try TLS.get_exn tls_k_context with TLS.Not_set -> Hmap.empty);
    with_context =
      (fun ctx f ->
        let old =
          try TLS.get_exn tls_k_context with TLS.Not_set -> Hmap.empty
        in
        let finally () = TLS.set tls_k_context old in
        TLS.set tls_k_context ctx;
        Fun.protect ~finally f);
  }
