(** Unix timestamp.

    These timestamps measure time since the Unix epoch (jan 1, 1970) UTC in
    nanoseconds. *)

type t = int64

open struct
  let ns_in_a_day = Int64.(mul 1_000_000_000L (of_int (24 * 3600)))
end

(** Current unix timestamp in nanoseconds *)
let[@inline] now_unix_ns () : t =
  let span = Ptime_clock.now () |> Ptime.to_span in
  let d, ps = Ptime.Span.to_d_ps span in
  let d = Int64.(mul (of_int d) ns_in_a_day) in
  let ns = Int64.(div ps 1_000L) in
  Int64.(add d ns)

let pp_debug out (self : t) =
  let d = Int64.(to_int (div self ns_in_a_day)) in
  let ns = Int64.(rem self ns_in_a_day) in
  let ps = Int64.(mul ns 1_000L) in
  match Ptime.Span.of_d_ps (d, ps) with
  | None -> Format.fprintf out "ts: <%Ld ns>" self
  | Some span ->
    (match Ptime.add_span Ptime.epoch span with
    | None -> Format.fprintf out "ts: <%Ld ns>" self
    | Some ptime -> Ptime.pp_human () out ptime)
