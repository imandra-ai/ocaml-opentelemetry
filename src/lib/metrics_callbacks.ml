open Common_

type t = { cbs: (unit -> Metrics.t list) Alist.t } [@@unboxed]

let create () : t = { cbs = Alist.make () }

let[@inline] add_metrics_cb (self : t) f = Alist.add self.cbs f

let add_to_exporter (exp : Exporter.t) (self : t) =
  let on_tick () =
    (* collect all metrics *)
    let res = ref [] in
    List.iter
      (fun f ->
        let f_metrics = f () in
        res := List.rev_append f_metrics !res)
      (Alist.get self.cbs);
    let metrics = !res in

    (* emit the metrics *)
    Exporter.send_metrics exp metrics
  in
  Exporter.on_tick exp on_tick

let with_set_added_to_exporter (exp : Exporter.t) (f : t -> 'a) : 'a =
  let set = create () in
  add_to_exporter exp set;
  f set

let with_set_added_to_main_exporter (f : t -> unit) : unit =
  match Main_exporter.get () with
  | None -> ()
  | Some exp -> with_set_added_to_exporter exp f

module Main_set = struct
  let cur_set_ : t option Atomic.t = Atomic.make None

  let rec get () : t =
    match Atomic.get cur_set_ with
    | Some s -> s
    | None ->
      let s = create () in
      if Atomic.compare_and_set cur_set_ None (Some s) then
        s
      else
        get ()
end
