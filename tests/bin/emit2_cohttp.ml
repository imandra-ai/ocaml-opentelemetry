module T = Trace_lwt
module Otel = Opentelemetry
open Lwt.Syntax

let spf = Printf.sprintf

let ( let@ ) f x = f x

let sleep_inner = ref 0.1

let sleep_outer = ref 2.0

let n_jobs = ref 1

let num_sleep = Atomic.make 0

let stress_alloc_ = ref true

let stop = Atomic.make false

let num_tr = Atomic.make 0

let run_job () : unit Lwt.t =
  let i = ref 0 in
  while%lwt not @@ Atomic.get stop do
    let@ _sc =
      T.with_span_lwt ~__FILE__ ~__LINE__ "loop.outer" ~data:(fun () ->
          [ "i", `Int !i ])
    in

    let run_sub n ~j =
      (* parent scope is found via thread local storage *)
      let@ _sc =
        T.with_span_lwt ~__FILE__ ~__LINE__ "inside.job" ~data:(fun () ->
            [ "j", `Int j ])
      in

      let* () = Lwt_unix.sleep (!sleep_outer +. (float n *. 0.01)) in
      Atomic.incr num_sleep;

      T.messagef (fun k -> k "inner at %d" j);

      incr i;

      let run_sub_or_fail () =
        Atomic.incr num_tr;
        let@ _span = T.with_span_lwt ~__FILE__ ~__LINE__ "alloc" in

        (* allocate some stuff *)
        if !stress_alloc_ then (
          let _arr = Sys.opaque_identity @@ Array.make (25 * 25551) 42.0 in
          ignore _arr
        );

        let* () = Lwt_unix.sleep !sleep_inner in
        Atomic.incr num_sleep;

        if j = 4 && !i mod 13 = 0 then failwith "oh no";

        (* simulate a failure *)
        (* TODO: do we need [Trace.add_msg_to_span …]?
           T.Trace.add_event scope (fun () -> T.Event.make "done with alloc");
        *)
        Lwt.return ()
      in

      let run_sub () =
        try%lwt run_sub_or_fail () with Failure _ -> Lwt.return ()
      in

      let sub1 = run_sub () in
      let sub2 = run_sub () in
      let sub3 = run_sub () in

      let* () = sub1 and* () = sub2 and* () = sub3 in
      Lwt.return ()
    in

    for%lwt j = 0 to 4 do
      (* parent scope is found via thread local storage *)
      let@ _sc =
        T.with_span_lwt ~__FILE__ ~__LINE__ "loop.inner" ~data:(fun () ->
            [ "j", `Int j ])
      in

      let sub1 = run_sub 1 ~j in
      let sub2 = run_sub 2 ~j in
      let sub3 = run_sub 3 ~j in

      let* () = sub1 and* () = sub2 and* () = sub3 in
      Lwt.return ()
    done
  done

let run () : unit Lwt.t =
  Printf.printf "collector is on %S\n%!"
    (Opentelemetry_client_cohttp_lwt.get_url ());
  Otel.GC_metrics.basic_setup ();

  Otel.Metrics_callbacks.register (fun () ->
      Otel.Metrics.
        [
          sum ~name:"num-sleep" ~is_monotonic:true
            [ int (Atomic.get num_sleep) ];
        ]);

  let n_jobs = max 1 !n_jobs in
  Printf.printf "run %d jobs\n%!" n_jobs;

  let jobs = Array.init n_jobs (fun _ -> run_job ()) |> Array.to_list in
  Lwt.join jobs

let () =
  Sys.catch_break true;
  Otel.Globals.service_name := "t2";
  Otel.Globals.service_namespace := Some "ocaml-otel.test";
  let ts_start = Unix.gettimeofday () in

  let debug = ref false in
  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--stress-alloc",
        Arg.Bool (( := ) stress_alloc_),
        " perform heavy allocs in inner loop" );
      "--batch-traces", Arg.Int (( := ) batch_traces), " size of traces batch";
      ( "--batch-metrics",
        Arg.Int (( := ) batch_metrics),
        " size of metrics batch" );
      "--sleep-inner", Arg.Set_float sleep_inner, " sleep (in s) in inner loop";
      "--sleep-outer", Arg.Set_float sleep_outer, " sleep (in s) in outer loop";
      "-j", Arg.Set_int n_jobs, " number of parallel jobs";
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
    Opentelemetry_client_cohttp_lwt.Config.make ~debug:!debug
      ~batch_traces:(some_if_nzero batch_traces)
      ~batch_metrics:(some_if_nzero batch_metrics)
      ()
  in
  Format.printf "@[<2>sleep outer: %.3fs,@ sleep inner: %.3fs,@ config: %a@]@."
    !sleep_outer !sleep_inner Opentelemetry_client_cohttp_lwt.Config.pp config;

  let@ () =
    Fun.protect ~finally:(fun () ->
        let elapsed = Unix.gettimeofday () -. ts_start in
        let n_per_sec = float (Atomic.get num_tr) /. elapsed in
        Printf.printf "\ndone. %d spans in %.4fs (%.4f/s)\n%!"
          (Atomic.get num_tr) elapsed n_per_sec)
  in
  Opentelemetry_client_cohttp_lwt.with_setup ~stop ~config () @@ fun () ->
  Opentelemetry_trace.setup ();
  Lwt_main.run @@ run ()
