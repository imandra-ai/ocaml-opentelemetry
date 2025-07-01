type 'a t = {
  mutable size: int;
  mutable q: 'a list list;
  batch: int; (* Minimum size to batch before popping *)
  high_watermark: int;
  timeout: Mtime.span option;
  mutable start: Mtime.t;
}

let default_high_watermark batch_size =
  if batch_size = 1 then
    100
  else
    batch_size * 10

let make ?(batch = 1) ?high_watermark ?now ?timeout () : _ t =
  let high_watermark =
    match high_watermark with
    | Some x -> x
    | None -> default_high_watermark batch
  in
  let start =
    match now with
    | Some x -> x
    | None -> Mtime_clock.now ()
  in
  assert (batch > 0);
  { size = 0; q = []; start; batch; timeout; high_watermark }

let timeout_expired_ ~now self : bool =
  match self.timeout with
  | Some t ->
    let elapsed = Mtime.span now self.start in
    Mtime.Span.compare elapsed t >= 0
  | None -> false

(* Big enough to send a batch *)
let is_full_ self : bool = self.size >= self.batch

let ready_to_pop ~force ~now self =
  self.size > 0 && (force || is_full_ self || timeout_expired_ ~now self)

let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
  if ready_to_pop ~force ~now self then (
    let l = self.q in
    self.q <- [];
    self.size <- 0;
    assert (l <> []);
    let ls = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
    Some ls
  ) else
    None

let push (self : _ t) x : [ `Dropped | `Ok ] =
  if self.size >= self.high_watermark then
    (* drop this to prevent queue from growing too fast *)
    `Dropped
  else (
    if self.size = 0 && Option.is_some self.timeout then
      (* current batch starts now *)
      self.start <- Mtime_clock.now ();

    (* add to queue *)
    self.size <- 1 + self.size;
    self.q <- x :: self.q;
    `Ok
  )
