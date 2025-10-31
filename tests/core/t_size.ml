(* test the size of serialized data *)

open Opentelemetry

let res1 : Proto.Metrics.resource_metrics =
  Proto.Metrics.make_resource_metrics
    ~scope_metrics:
      [
        Proto.Metrics.make_scope_metrics
          ~metrics:
            [
              Metrics.sum ~name:"sum.foo"
                [
                  Metrics.int ~start_time_unix_nano:42L ~now:45L 10;
                  Metrics.int ~start_time_unix_nano:52L ~now:55L 20;
                ];
              Metrics.gauge ~name:"gauge.bar"
                [
                  Metrics.float ~start_time_unix_nano:42L ~now:45L 10.;
                  Metrics.float ~start_time_unix_nano:52L ~now:55L 20.;
                ];
            ]
          ();
      ]
    ()

let str =
  let enc = Pbrt.Encoder.create () in
  Proto.Metrics.encode_pb_resource_metrics res1 enc;
  Pbrt.Encoder.to_string enc

let () = Printf.printf "metrics size: %dB\n" (String.length str)

let () =
  let dec = Pbrt.Decoder.of_string str in
  let res2 = Proto.Metrics.decode_pb_resource_metrics dec in
  Format.printf "res1: %a@." Proto.Metrics.pp_resource_metrics res1;
  Format.printf "res1: %a@." Proto.Metrics.pp_resource_metrics res2;
  ()

(* traces *)

let trace1 : Proto.Trace.resource_spans =
  let span_id = Span_id.dummy |> Span_id.to_bytes in
  let trace_id = Trace_id.dummy |> Trace_id.to_bytes in
  Proto.Trace.make_resource_spans
    ~scope_spans:
      [
        Proto.Trace.make_scope_spans
          ~spans:
            [
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp1"
                ~start_time_unix_nano:10L ~end_time_unix_nano:15L ~events:[]
                ~links:[] ~attributes:[] ();
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp2"
                ~start_time_unix_nano:20L ~end_time_unix_nano:25L ~events:[]
                ~links:[] ~attributes:[] ~parent_span_id:span_id ();
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp3"
                ~start_time_unix_nano:30L ~end_time_unix_nano:35L ~events:[]
                ~links:[] ~attributes:[] ~parent_span_id:span_id ();
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp4"
                ~start_time_unix_nano:40L ~end_time_unix_nano:45L ~events:[]
                ~links:[] ~attributes:[] ~parent_span_id:span_id ();
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp5"
                ~start_time_unix_nano:50L ~end_time_unix_nano:55L ~events:[]
                ~links:[] ~attributes:[] ~parent_span_id:span_id ();
              Proto.Trace.make_span ~trace_id ~span_id ~name:"sp6"
                ~start_time_unix_nano:60L ~end_time_unix_nano:65L ~events:[]
                ~links:[] ~attributes:[] ~parent_span_id:span_id ();
            ]
          ();
      ]
    ()

let str =
  let enc = Pbrt.Encoder.create () in
  Proto.Trace.encode_pb_resource_spans trace1 enc;
  Pbrt.Encoder.to_string enc

let () = Printf.printf "trace size: %dB\n" (String.length str)

let () =
  let dec = Pbrt.Decoder.of_string str in
  let trace2 = Proto.Trace.decode_pb_resource_spans dec in
  Format.printf "trace1: %a@." Proto.Trace.pp_resource_spans trace1;
  Format.printf "trace2: %a@." Proto.Trace.pp_resource_spans trace2;
  ()
