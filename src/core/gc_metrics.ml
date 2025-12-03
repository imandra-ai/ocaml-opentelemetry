open Common_

open struct
  let[@inline] bytes_per_word = Sys.word_size / 8

  let[@inline] word_to_bytes n = n * bytes_per_word

  let[@inline] word_to_bytes_f n = n *. float bytes_per_word
end

let get_metrics () : Metrics.t list =
  let gc = Gc.quick_stat () in
  let now = Timestamp_ns.now_unix_ns () in
  let open Metrics in
  let open Conventions.Metrics in
  [
    gauge ~name:Process.Runtime.Ocaml.GC.major_heap ~unit_:"B"
      [ int ~now (word_to_bytes gc.Gc.heap_words) ];
    sum ~name:Process.Runtime.Ocaml.GC.minor_allocated
      ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
      ~is_monotonic:true ~unit_:"B"
      [ float ~now (word_to_bytes_f gc.Gc.minor_words) ];
    sum ~name:Process.Runtime.Ocaml.GC.minor_collections
      ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
      ~is_monotonic:true
      [ int ~now gc.Gc.minor_collections ];
    sum ~name:Process.Runtime.Ocaml.GC.major_collections
      ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
      ~is_monotonic:true
      [ int ~now gc.Gc.major_collections ];
    sum ~name:Process.Runtime.Ocaml.GC.compactions
      ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
      ~is_monotonic:true
      [ int ~now gc.Gc.compactions ];
  ]

let setup (exp : #Exporter.t) =
  let on_tick () =
    let m = get_metrics () in
    exp#send_metrics m
  in
  Exporter.on_tick exp on_tick

let setup_on_main_exporter () =
  match Exporter.Main_exporter.get () with
  | None -> ()
  | Some exp -> setup exp

let basic_setup = setup_on_main_exporter
