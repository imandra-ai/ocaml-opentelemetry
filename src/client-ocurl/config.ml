type t = {
  bg_threads: int;
      (** Are there background threads, and how many? Default [4]. This will be
          adjusted to be at least [1] and at most [32]. *)
  ticker_thread: bool;
      (** If true, start a thread that regularly checks if signals should be
          sent to the collector. Default [true] *)
  ticker_interval_ms: int;
      (** Interval for ticker thread, in milliseconds. This is only useful if
          [ticker_thread] is [true]. This will be clamped between [2 ms] and
          some longer interval (maximum [60s] currently). Default 500.
          @since 0.7 *)
  common: Client.Config.t;
      (** Common configuration options
          @since 0.12*)
}

let pp fmt _ = Format.pp_print_string fmt "TODO"

module Env = Client.Config.Env ()

let make =
  Env.make
    (fun
      common
      ?(bg_threads = 4)
      ?(ticker_thread = true)
      ?(ticker_interval_ms = 500)
      ()
    -> { bg_threads; ticker_thread; ticker_interval_ms; common })
