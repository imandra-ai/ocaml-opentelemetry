module Client = Opentelemetry_client
module L = Opentelemetry_proto.Logs

(* NOTE: This port must be different from that used by other integration tests,
   to prevent socket binding clashes. *)
let port = 4359

let url = Printf.sprintf "http://localhost:%d" port

let cmd = [ "emit_logs_cohttp"; "--url"; url ]

let tests (signal_batches : Client.Signal.t list) =
  ignore signal_batches;
  List.iter
    (fun (signal_batch : Client.Signal.t) ->
      match signal_batch with
      | Logs ls ->
        ls (* Mask out the times so tests don't change in between runs *)
        |> List.map (fun (l : L.resource_logs) ->
               let masked_scope_logs =
                 List.map
                   (fun (sl : L.scope_logs) ->
                     let masked_log_records =
                       List.map
                         (fun (lr : L.log_record) ->
                           {
                             lr with
                             time_unix_nano = 0L;
                             observed_time_unix_nano = 0L;
                           })
                         sl.log_records
                     in
                     { sl with log_records = masked_log_records })
                   l.scope_logs
               in
               { l with scope_logs = masked_scope_logs })
        |> List.iter (Format.printf "%a\n" L.pp_resource_logs)
      | _ -> ())
    signal_batches

let () =
  let signal_batches = Signal_gatherer.gather_signals ~port cmd in
  tests signal_batches
