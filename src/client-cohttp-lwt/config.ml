type t = Opentelemetry_client.Config.t

module Env = Opentelemetry_client.Config.Env ()

let pp = Opentelemetry_client.Config.pp

let make = Env.make (fun common () -> common)
