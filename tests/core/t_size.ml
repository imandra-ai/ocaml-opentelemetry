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
