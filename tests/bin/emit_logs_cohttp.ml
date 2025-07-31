(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module T = Opentelemetry_lwt
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let string_tag = Logs.Tag.def "string_attr" Format.pp_print_string

let int_tag = Logs.Tag.def "int_attr" Format.pp_print_int

let float_tag = Logs.Tag.def "float_attr" Format.pp_print_float

let bool_tag = Logs.Tag.def "bool_attr" Format.pp_print_bool

let string_list_tag =
  Logs.Tag.def "string_list_attr" (Format.pp_print_list Format.pp_print_string)

let varied_tag_set =
  Logs.Tag.(
    empty
    |> add string_tag "string_value"
    |> add int_tag 42 |> add float_tag 3.14 |> add bool_tag true
    |> add string_list_tag [ "foo"; "bar"; "baz" ])

let run () =
  let otel_reporter =
    Opentelemetry_logs.otel_reporter ~service_name:"emit_logs"
      ~attributes:[ "my_reporter_attr", `String "foo" ]
      ()
  in
  Logs.set_reporter otel_reporter;
  Logs.set_level (Some Logs.Debug);
  Logs.debug (fun m -> m "emit_logs: starting");
  Logs.info (fun m -> m "emit_logs: info log");
  Logs.warn (fun m -> m "emit_logs: warn log");
  Logs.err (fun m -> m "emit_logs: error log");
  Logs.app (fun m -> m "emit_logs: app log");
  let%lwt () =
    T.Trace.with_ ~kind:T.Span.Span_kind_producer "my_scope" (fun _scope ->
        Logs.info (fun m ->
            m ~tags:varied_tag_set
              "emit_logs: this log is emitted with varied tags from a span");
        Lwt.return_unit)
  in
  let no_emit_tag = Opentelemetry_logs.emit_telemetry false in
  Logs.info (fun m ->
      m ~tags:no_emit_tag "emit_logs: this log will not be emitted");
  Logs.info (fun m ->
      m ~tags:varied_tag_set
        "emit_logs: this log will be emitted with varied tags");

  let fmt_logger = Logs_fmt.reporter ~dst:Format.err_formatter () in
  let combined_logger =
    Opentelemetry_logs.attach_otel_reporter ~service_name:"emit_logs_fmt"
      ~attributes:[ "my_fmt_attr", `String "bar" ]
      fmt_logger
  in
  Logs.set_reporter combined_logger;
  Logs.info (fun m ->
      m "emit_logs: this log will be emitted from otel and fmt reporter");
  Logs.set_level None;
  (* disable logging *)
  Logs.debug (fun m ->
      m "emit_logs: this log will not be emitted, logging disabled");
  Lwt.return_unit

let () =
  Sys.catch_break true;
  T.Globals.service_name := "t1";
  T.Globals.service_namespace := Some "ocaml-otel.test";

  let debug = ref false in
  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let batch_logs = ref 400 in
  let url = ref None in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--url",
        Arg.String (fun s -> url := Some s),
        " set the url for the OTel collector" );
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
    Opentelemetry_client_cohttp_lwt.Config.make ~debug:!debug ?url:!url
      ~batch_traces:(some_if_nzero batch_traces)
      ~batch_metrics:(some_if_nzero batch_metrics)
      ~batch_logs:(some_if_nzero batch_logs) ()
  in
  Format.printf "@[@ config: %a@]@." Opentelemetry_client_cohttp_lwt.Config.pp
    config;

  Opentelemetry_client_cohttp_lwt.with_setup ~config () run |> Lwt_main.run
