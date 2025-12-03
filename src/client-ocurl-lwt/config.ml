type t = Opentelemetry_client.Client_config.t

module Env = Opentelemetry_client.Client_config.Env ()

let pp = Opentelemetry_client.Client_config.pp

let make = Env.make (fun common () -> common)
