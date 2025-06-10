type t = Client.Config.t

module Env = Client.Config.Env ()

let pp = Client.Config.pp

let make = Env.make (fun common () -> common)
