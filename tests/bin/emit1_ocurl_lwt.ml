module OT = Opentelemetry
module Atomic = Opentelemetry_atomic.Atomic

let spf = Printf.sprintf

let ( let@ ) = ( @@ )

let sleep_inner = ref 0.1

let sleep_outer = ref 2.0

let n_jobs = ref 1

let n = ref max_int

let num_sleep = Atomic.make 0

let stress_alloc_ = ref true

let num_tr = Atomic.make 0

let run_job () : unit Lwt.t =
  let active = OT.Main_exporter.active () in
  let tracer = OT.Tracer.get_main () in
  let i = ref 0 in
  let cnt = ref 0 in

  while%lwt OT.Aswitch.is_on active && !cnt < !n do
    let@ _scope =
      Atomic.incr num_tr;
      OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_producer "loop.outer"
        ~attrs:[ "i", `Int !i ]
    in

    (* Printf.printf "cnt=%d\n%!" !cnt; *)
    incr cnt;

    for%lwt j = 0 to 4 do
      (* parent scope is found via thread local storage *)
      let@ scope =
        Atomic.incr num_tr;
        OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_internal ~parent:_scope
          ~attrs:[ "j", `Int j ]
          "loop.inner"
      in

      if !sleep_outer > 0. then (
        Unix.sleepf !sleep_outer;
        Atomic.incr num_sleep
      );

      let logger = OT.Logger.get_main () in
      OT.Emitter.emit logger
        [
          OT.Log_record.make_strf ~trace_id:(OT.Span.trace_id scope)
            ~span_id:(OT.Span.id scope) ~severity:Severity_number_info
            "inner at %d" j;
        ];

      incr i;

      (try
         (* allocate some stuff *)
         if !stress_alloc_ then (
           let@ _ =
             OT.Tracer.with_ tracer ~kind:OT.Span.Span_kind_internal
               ~parent:scope "alloc"
           in
           Atomic.incr num_tr;

           let _arr : _ array =
             Sys.opaque_identity @@ Array.make (25 * 25551) 42.0
           in
           ignore _arr
         );

         if !sleep_inner > 0. then (
           Unix.sleepf !sleep_inner;
           Atomic.incr num_sleep
         );

         if j = 4 && !i mod 13 = 0 then failwith "oh no";

         (* simulate a failure *)
         OT.Span.add_event scope (OT.Event.make "done with alloc")
       with Failure _ -> ());

      Lwt.return ()
    done
  done

let run () : unit Lwt.t =
  OT.Gc_metrics.setup_on_main_exporter ();

  OT.Metrics_callbacks.with_set_added_to_main_exporter (fun set ->
      OT.Metrics_callbacks.add_metrics_cb set OT.Main_exporter.self_metrics;
      OT.Metrics_callbacks.add_metrics_cb set (fun () ->
          OT.Metrics.
            [
              sum ~name:"num-sleep" ~is_monotonic:true
                [ int (Atomic.get num_sleep) ];
            ]));

  let n_jobs = max 1 !n_jobs in
  Printf.printf "run %d job(s)\n%!" n_jobs;

  let jobs =
    List.init n_jobs (fun _ -> try run_job () with Sys.Break -> Lwt.return ())
  in
  Lwt.join jobs

let () =
  OT.Globals.service_name := "t1";
  OT.Globals.service_namespace := Some "ocaml-otel.test";
  let ts_start = Unix.gettimeofday () in

  let debug = ref false in

  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let batch_logs = ref 400 in
  let self_trace = ref true in
  let final_stats = ref false in

  let n_bg_threads = ref 0 in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--stress-alloc",
        Arg.Bool (( := ) stress_alloc_),
        " perform heavy allocs in inner loop" );
      ( "--batch-metrics",
        Arg.Int (( := ) batch_metrics),
        " size of metrics batch" );
      "--batch-traces", Arg.Int (( := ) batch_traces), " size of traces batch";
      "--batch-logs", Arg.Int (( := ) batch_logs), " size of logs batch";
      "--sleep-inner", Arg.Set_float sleep_inner, " sleep (in s) in inner loop";
      "--sleep-outer", Arg.Set_float sleep_outer, " sleep (in s) in outer loop";
      "-j", Arg.Set_int n_jobs, " number of parallel jobs";
      "--bg-threads", Arg.Set_int n_bg_threads, " number of background threads";
      "--no-self-trace", Arg.Clear self_trace, " disable self tracing";
      "-n", Arg.Set_int n, " number of iterations (default ∞)";
      "--final-stats", Arg.Set final_stats, " display some metrics at the end";
    ]
    |> Arg.align
  in

  Arg.parse opts (fun _ -> ()) "emit1 [opt]*";

  let some_if_nzero r =
    if !r > 0 then
      Some !r
    else
      None
  in
  let config =
    Opentelemetry_client_ocurl_lwt.Config.make ~debug:!debug
      ~self_trace:!self_trace
      ?http_concurrency_level:(some_if_nzero n_bg_threads)
      ~batch_traces:(some_if_nzero batch_traces)
      ~batch_metrics:(some_if_nzero batch_metrics)
      ~batch_logs:(some_if_nzero batch_logs) ()
  in
  Format.printf "@[<2>sleep outer: %.3fs,@ sleep inner: %.3fs,@ config: %a@]@."
    !sleep_outer !sleep_inner Opentelemetry_client_ocurl_lwt.Config.pp config;

  let finally () =
    let elapsed = Unix.gettimeofday () -. ts_start in
    let n_per_sec = float (Atomic.get num_tr) /. elapsed in
    Printf.printf "\ndone. %d spans in %.4fs (%.4f/s)\n%!" (Atomic.get num_tr)
      elapsed n_per_sec
  in

  Lwt_main.run
  @@
  let@ () = Fun.protect ~finally in
  Opentelemetry_client_ocurl_lwt.with_setup ~config () run
