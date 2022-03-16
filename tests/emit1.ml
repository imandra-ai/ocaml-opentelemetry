
module T = Opentelemetry

let run () =
  Printf.printf "collector is on %S\n%!" (Opentelemetry_client_ocurl.get_url());
  let i = ref 0 in
  while true do
    Unix.sleepf 2.;

    let gc = Gc.stat() in
    T.Metrics.(
      emit [
        gauge ~name:"i" [int !i];
        gauge ~name:"major-heap-words" [int gc.Gc.heap_words];
        sum ~name:"minor-allocated" [float gc.Gc.minor_words];
      ]);

    incr i;

    (* allocate some stuff *)
    let _arr = Sys.opaque_identity @@ Array.make (25 * 25551) 42.0 in
    ignore _arr;
  done

let () =
  Opentelemetry_client_ocurl.with_setup run
