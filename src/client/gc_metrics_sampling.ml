module OT = Opentelemetry
module AList = Opentelemetry.AList

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

let gc_metrics = AList.make ()

let[@inline] signal_we_need_gc_metrics () = Atomic.set needs_gc_metrics true

let[@inline] pop_gc_metrics () = AList.pop_all gc_metrics

(* capture current GC metrics if {!needs_gc_metrics} is true,
   or it has been a long time since the last GC metrics collection,
   and push them into {!gc_metrics} for later collection *)
let sample_gc_metrics_if_needed () =
  let now = lazy (Mtime_clock.now ()) in
  let needs_gc_metrics_true = Atomic.exchange needs_gc_metrics false in

  let[@inline] timeout () =
    let (lazy now) = now in
    let elapsed = Mtime.span now (Atomic.get last_gc_metrics) in
    Mtime.Span.compare elapsed timeout_gc_metrics > 0
  in
  if needs_gc_metrics_true || timeout () then (
    let (lazy now) = now in
    Atomic.set last_gc_metrics now;
    let l =
      OT.Metrics.make_resource_metrics
        ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
      @@ Opentelemetry.GC_metrics.get_metrics ()
    in
    AList.add gc_metrics l
  )
