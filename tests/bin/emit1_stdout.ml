module OT = Opentelemetry
module OTC = Opentelemetry_client

let spf = Printf.sprintf

let ( let@ ) = ( @@ )

let sleep_inner = ref 0.1

let sleep_outer = ref 2.0

let n_jobs = ref 1

let n = ref max_int

let num_sleep = Atomic.make 0

let num_tr = Atomic.make 0

let run_job () =
  let active = OT.Main_exporter.active () in
  let tracer = OT.Tracer.get_main () in
  let i = ref 0 in
  let cnt = ref 0 in

  while OT.Aswitch.is_on active && !cnt < !n do
    let@ _scope =
      Atomic.incr num_tr;
      OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_producer "loop.outer"
        ~attrs:[ "i", `Int !i ]
    in

    (* Printf.printf "cnt=%d\n%!" !cnt; *)
    incr cnt;

    for j = 0 to 4 do
      (* parent scope is found via thread local storage *)
      let@ scope =
        Atomic.incr num_tr;
        OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_internal ~parent:_scope
          ~attrs:[ "j", `Int j ]
          "loop.inner"
      in

      Unix.sleepf !sleep_outer;
      Atomic.incr num_sleep;

      let logger = OT.Logger.get_main () in
      OT.Emitter.emit logger
        [
          OT.Log_record.make_strf ~trace_id:(OT.Span.trace_id scope)
            ~span_id:(OT.Span.id scope) ~severity:Severity_number_info
            "inner at %d" j;
        ];

      incr i;

      try
        Atomic.incr num_tr;
        let@ _ =
          OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_internal ~parent:scope
            "alloc"
        in

        Unix.sleepf !sleep_inner;
        Atomic.incr num_sleep;

        if j = 4 && !i mod 13 = 0 then failwith "oh no";

        (* simulate a failure *)
        OT.Span.add_event scope (OT.Event.make "done with alloc")
      with Failure _ -> ()
    done
  done

let run () =
  OT.Gc_metrics.setup_on_main_exporter ();

  OT.Metrics_callbacks.with_set_added_to_main_exporter (fun set ->
      OT.Metrics_callbacks.add_metrics_cb set (fun () ->
          OT.Metrics.
            [
              sum ~name:"num-sleep" ~is_monotonic:true
                [ int (Atomic.get num_sleep) ];
            ]));

  let n_jobs = max 1 !n_jobs in
  Printf.printf "run %d job(s)\n%!" n_jobs;

  let jobs =
    Array.init n_jobs (fun _ ->
        let job () = try run_job () with Sys.Break -> () in
        Thread.create job ())
  in
  Array.iter Thread.join jobs

module Consumer_exporter =
  OTC.Generic_consumer_exporter.Make (OTC.Io_sync) (OTC.Notifier_sync)

let () =
  OT.Globals.service_name := "t1";
  OT.Globals.service_namespace := Some "ocaml-otel.test";
  let ts_start = Unix.gettimeofday () in

  let debug = ref false in

  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let batch_logs = ref 400 in
  let queued = ref false in

  let n_bg_threads = ref 0 in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--batch-metrics",
        Arg.Int (( := ) batch_metrics),
        " size of metrics batch" );
      "--batch-traces", Arg.Int (( := ) batch_traces), " size of traces batch";
      "--batch-logs", Arg.Int (( := ) batch_logs), " size of logs batch";
      "--sleep-inner", Arg.Set_float sleep_inner, " sleep (in s) in inner loop";
      "--sleep-outer", Arg.Set_float sleep_outer, " sleep (in s) in outer loop";
      "-j", Arg.Set_int n_jobs, " number of parallel jobs";
      "--bg-threads", Arg.Set_int n_bg_threads, " number of background threads";
      "-n", Arg.Set_int n, " number of iterations (default ∞)";
      "--queued", Arg.Set queued, " queue exporter";
    ]
    |> Arg.align
  in

  Arg.parse opts (fun _ -> ()) "emit1 [opt]*";

  Format.printf "@[<2>sleep outer: %.3fs,@ sleep inner: %.3fs,@ queued: %ba@]@."
    !sleep_outer !sleep_inner !queued;

  let exporter =
    let exp = OTC.Exporter_stdout.stdout in
    if !queued then (
      let q = OTC.Bounded_queue_sync.create ~high_watermark:20_000 () in
      OTC.Exporter_queued.create ~q
        ~consumer:(Consumer_exporter.consumer exp)
        ()
    ) else
      exp
  in

  OT.Main_exporter.set exporter;

  let@ () =
    Fun.protect ~finally:(fun () ->
        let elapsed = Unix.gettimeofday () -. ts_start in
        let n_per_sec = float (Atomic.get num_tr) /. elapsed in
        Printf.printf "\ndone. %d spans in %.4fs (%.4f/s)\n%!"
          (Atomic.get num_tr) elapsed n_per_sec)
  in
  run ()
