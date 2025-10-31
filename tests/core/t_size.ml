(* test the size of serialized data *)

open Opentelemetry

let m =
  Metrics.make_resource_metrics
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

let () =
  let str = Opentelemetry_client.Signal.Encode.metrics [ m; m ] in
  Printf.printf "metrics size: %dB\n" (String.length str)
