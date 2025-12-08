open Alcotest
module Config = Opentelemetry_client.Client_config

let test_config_printing () =
  let module Env = Config.Env () in
  let actual =
    Format.asprintf "%a" Config.pp @@ Env.make (fun common () -> common) ()
  in
  let expected =
    "{ debug=false;\n\
    \ self_trace=false; url_traces=\"http://localhost:4318/v1/traces\";\n\
    \ url_metrics=\"http://localhost:4318/v1/metrics\";\n\
    \ url_logs=\"http://localhost:4318/v1/logs\"; headers=[]; batch_traces=400;\n\
    \ batch_metrics=20; batch_logs=400; batch_timeout_ms=2000;\n\
    \ http_concurrency_level=None }"
  in
  check' string ~msg:"is rendered correctly" ~actual ~expected

let test_overriding_stateful_config () =
  let module Env = Config.Env () in
  Env.set_headers [ "foo", "bar" ];
  Env.set_debug true;
  let headers = [ "changed", "header" ] in
  let debug = false in
  let config : Config.t =
    Env.make (fun common () -> common) ~debug ~headers ()
  in
  check'
    (list (pair string string))
    ~msg:"header is overriden" ~actual:(Env.get_headers ()) ~expected:headers;
  check'
    (list (pair string string))
    ~msg:"config and stateful headers are the same" ~actual:(Env.get_headers ())
    ~expected:config.headers;
  check' bool ~msg:"debug is overriden" ~actual:(Env.get_debug ())
    ~expected:debug;
  check' bool ~msg:"config and stateful debug are the same"
    ~actual:(Env.get_debug ()) ~expected:config.debug

let suite =
  [
    test_case "default config pretty printing" `Quick test_config_printing;
    test_case "overriding default stateful values via make constructor" `Quick
      test_overriding_stateful_config;
  ]

let () = Alcotest.run "Opentelemetry_client" [ "Config", suite ]
