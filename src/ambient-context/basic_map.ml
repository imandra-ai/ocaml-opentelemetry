(** Extremely basic storage using a map from thread id to context *)

open Opentelemetry_ambient_context_core

open struct
  module Atomic = Opentelemetry_atomic.Atomic

  module Int_map = Map.Make (struct
    type t = int

    let compare : t -> t -> int = Stdlib.compare
  end)

  type st = { m: Context.t ref Int_map.t Atomic.t } [@@unboxed]

  let get (self : st) : Context.t =
    let tid = Thread.id @@ Thread.self () in
    match Int_map.find tid (Atomic.get self.m) with
    | exception Not_found -> Context.empty
    | ctx_ref -> !ctx_ref

  let with_context (self : st) ctx f =
    let tid = Thread.id @@ Thread.self () in

    let ctx_ref =
      try Int_map.find tid (Atomic.get self.m)
      with Not_found ->
        let r = ref Context.empty in
        while
          let m = Atomic.get self.m in
          let m' = Int_map.add tid r m in
          not (Atomic.compare_and_set self.m m m')
        do
          ()
        done;
        r
    in

    let old_ctx = !ctx_ref in
    ctx_ref := ctx;

    let finally () = ctx_ref := old_ctx in
    Fun.protect ~finally f
end

let create_storage () : Storage.t =
  let st = { m = Atomic.make Int_map.empty } in
  {
    name = "basic-map";
    get_context = (fun () -> get st);
    with_context = (fun ctx f -> with_context st ctx f);
  }

(** Default storage *)
let storage : Storage.t = create_storage ()
