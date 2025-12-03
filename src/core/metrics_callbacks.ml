open Common_

type t = { cbs: (unit -> Metrics.t list) AList.t } [@@unboxed]

let create () : t = { cbs = AList.make () }

let[@inline] add_metrics_cb (self : t) f = AList.add self.cbs f

let add_to_exporter (exp : #Exporter.t) (self : t) =
  let on_tick () =
    (* collect all metrics *)
    let res = ref [] in
    List.iter
      (fun f ->
        let f_metrics = f () in
        res := List.rev_append f_metrics !res)
      (AList.get self.cbs);
    let metrics = !res in

    (* emit the metrics *)
    Exporter.send_metrics exp metrics
  in
  Exporter.on_tick exp on_tick

module Main_set = struct
  let cur_set_ : t option Atomic.t = Atomic.make None

  let rec get () =
    match Atomic.get cur_set_ with
    | Some s -> s
    | None ->
      let s = create () in
      if Atomic.compare_and_set cur_set_ None (Some s) then
        s
      else
        get ()
end
