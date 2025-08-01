module OT = Opentelemetry
module Atomic = Opentelemetry_atomic.Atomic

let spf = Printf.sprintf

let ( let@ ) f x = f x

let sleep_inner = ref 0.1

let sleep_outer = ref 2.0

let n_jobs = ref 1

let iterations = Atomic.make 1

let num_sleep = Atomic.make 0

let stress_alloc_ = ref true

let stop = Atomic.make false

let num_tr = Atomic.make 0

(* Counter used to mark simulated failures *)
let i = ref 0

let run_job clock _job_id : unit =
  while not @@ Atomic.get stop do
    let@ scope =
      Atomic.incr num_tr;
      OT.Trace.with_ ~kind:OT.Span.Span_kind_producer "loop.outer"
        ~attrs:[ "i", `Int !i ]
    in

    for j = 0 to Atomic.get iterations do
      if j >= Atomic.get iterations then
        (* Terminate program, having reached our max iterations *)
        Atomic.set stop true
      else
        (* parent scope is found via thread local storage *)
        let@ scope =
          Atomic.incr num_tr;
          OT.Trace.with_ ~scope ~kind:OT.Span.Span_kind_internal
            ~attrs:[ "j", `Int j ]
            "loop.inner"
        in

        let () = Eio.Time.sleep clock !sleep_outer in
        Atomic.incr num_sleep;

        OT.Logs.(
          emit
            [
              make_strf ~trace_id:scope.trace_id ~span_id:scope.span_id
                ~severity:Severity_number_info "inner at %d" j;
            ]);

        incr i;

        try
          Atomic.incr num_tr;
          let@ scope =
            OT.Trace.with_ ~kind:OT.Span.Span_kind_internal ~scope "alloc"
          in
          (* allocate some stuff *)
          if !stress_alloc_ then (
            let _arr = Sys.opaque_identity @@ Array.make (25 * 25551) 42.0 in
            ignore _arr
          );

          let () = Eio.Time.sleep clock !sleep_inner in
          Atomic.incr num_sleep;

          if j = 4 && !i mod 13 = 0 then failwith "oh no";

          (* simulate a failure *)
          Opentelemetry.Scope.add_event scope (fun () ->
              OT.Event.make "done with alloc")
        with Failure _ -> ()
    done
  done

let run env proc () : unit =
  OT.GC_metrics.basic_setup ();

  OT.Metrics_callbacks.register (fun () ->
      OT.Metrics.
        [
          sum ~name:"num-sleep" ~is_monotonic:true
            [ int (Atomic.get num_sleep) ];
        ]);

  let n_jobs = max 1 !n_jobs in
  Printf.printf "run %d jobs in proc %d\n%!" n_jobs proc;

  Eio.Switch.run (fun sw ->
      for j = 1 to n_jobs do
        Eio.Fiber.fork ~sw (fun () -> run_job env#clock j)
      done)

let () =
  Sys.catch_break true;
  OT.Globals.service_name := "t1";
  OT.Globals.service_namespace := Some "ocaml-otel.test";
  let ts_start = Unix.gettimeofday () in

  let debug = ref false in
  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let batch_logs = ref 400 in
  let url = ref None in
  let n_procs = ref 1 in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--url",
        Arg.String (fun s -> url := Some s),
        " set the url for the OTel collector" );
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
      ( "--iterations",
        Arg.Int (Atomic.set iterations),
        " the number of iterations to run" );
      "-j", Arg.Set_int n_jobs, " number of jobs per processes";
      "--procs", Arg.Set_int n_procs, " number of processes";
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
    Opentelemetry_client_cohttp_eio.Config.make ~debug:!debug ?url:!url
      ~batch_traces:(some_if_nzero batch_traces)
      ~batch_metrics:(some_if_nzero batch_metrics)
      ~batch_logs:(some_if_nzero batch_logs) ()
  in
  Format.printf "@[<2>sleep outer: %.3fs,@ sleep inner: %.3fs,@ config: %a@]@."
    !sleep_outer !sleep_inner Opentelemetry_client_cohttp_eio.Config.pp config;

  let@ () =
    Fun.protect ~finally:(fun () ->
        let elapsed = Unix.gettimeofday () -. ts_start in
        let n_per_sec = float (Atomic.get num_tr) /. elapsed in
        Printf.printf "\ndone. %d spans in %.4fs (%.4f/s)\n%!"
          (Atomic.get num_tr) elapsed n_per_sec)
  in
  Eio_main.run @@ fun env ->
  (if !n_procs < 2 then
     Opentelemetry_client_cohttp_eio.with_setup ~stop ~config (run env 0) env
   else
     Eio.Switch.run @@ fun sw ->
     Opentelemetry_client_cohttp_eio.setup ~stop ~config ~sw env;
     let dm = Eio.Stdenv.domain_mgr env in
     Eio.Switch.run (fun sw ->
         for proc = 1 to !n_procs do
           Eio.Fiber.fork ~sw @@ fun () ->
           Eio.Domain_manager.run dm (run env proc)
         done));
  Opentelemetry.Collector.remove_backend () ~on_done:ignore
