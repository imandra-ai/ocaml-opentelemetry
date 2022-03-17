
module T = Opentelemetry
let spf = Printf.sprintf
let (let@) f x = f x

let run () =
  Printf.printf "collector is on %S\n%!" (Opentelemetry_client_ocurl.get_url());
  let i = ref 0 in
  while true do
    let@ (tr,sp) = T.Trace.with_ ~service_name:"t1" "outer.loop" ~attrs:["i", `Int !i] in

    for j=0 to 4 do

      let@ (tr,sp) = T.Trace.with_ ~service_name:"t1" ~attrs:["j", `Int j] "loop.inner" in
      Unix.sleepf 2.;

      let gc = Gc.stat() in
      T.Metrics.(
        emit [
          gauge ~name:"ocaml_opentracing.test.i" [int !i];
          gauge ~name:"ocaml_opentracing.test.major_heap_words" [int gc.Gc.heap_words];
          sum ~name:"ocaml_opentracing.test.minor_allocated" [float gc.Gc.minor_words];
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
    done;
  done

let () =
  Opentelemetry_client_ocurl.with_setup run
