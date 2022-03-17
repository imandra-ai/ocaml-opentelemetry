
module T = Opentelemetry

let (let@) f x = f x

let run () =
  Printf.printf "collector is on %S\n%!" (Opentelemetry_client_ocurl.get_url());
  let i = ref 0 in
  while true do
    let@ (tr,sp) = T.Trace.with_ "loop" in
    Unix.sleepf 2.;

    let gc = Gc.stat() in
    T.Metrics.(
      emit [
        gauge ~name:"i" [int !i];
        gauge ~name:"major-heap-words" [int gc.Gc.heap_words];
        sum ~name:"minor-allocated" [float gc.Gc.minor_words];
      ]);

    incr i;

    begin
      let@ _ = T.Trace.with_ ~kind:T.Span.Span_kind_client
          ~trace_id:tr ~parent:sp "alloc" in
      (* allocate some stuff *)
      let _arr = Sys.opaque_identity @@ Array.make (25 * 25551) 42.0 in
      ignore _arr;
      Unix.sleepf 0.1;
  end;
  done

let () =
  Opentelemetry_client_ocurl.with_setup run
