type 'a t = {
  mutable size: int;
  mutable q: 'a list;
      (** The queue is a FIFO represented as a list in reverse order *)
  batch: int;  (** Minimum size to batch before popping *)
  high_watermark: int;  (** Size above which we start dropping signals *)
  timeout: Mtime.span option;
  mutable start: Mtime.t;
  mutex: Mutex.t;
}

(* Mutex.protect was added in OCaml 5.1, but we want support back to 4.08 *)
(* cannot inline, otherwise flambda might move code around. (as per Stdlib) *)
let[@inline never] protect_mutex m f =
  Mutex.lock m;
  match f () with
  | x ->
    Mutex.unlock m;
    x
  | exception e ->
    (* NOTE: [unlock] does not poll for asynchronous exceptions *)
    Mutex.unlock m;
    Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())

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
  let mutex = Mutex.create () in
  assert (batch > 0);
  { size = 0; q = []; start; batch; timeout; high_watermark; mutex }

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
  let rev_batch_opt =
    protect_mutex self.mutex @@ fun () ->
    if ready_to_pop ~force ~now self then (
      assert (self.q <> []);
      let batch = self.q in
      self.q <- [];
      self.size <- 0;
      Some batch
    ) else
      None
  in
  match rev_batch_opt with
  | None -> None
  | Some batch ->
    (* Reverse the list to retrieve the FIFO order. *)
    Some (List.rev batch)

let rec push_unprotected (self : _ t) ~(elems : _ list) : unit =
  match elems with
  | [] -> ()
  | x :: xs ->
    self.q <- x :: self.q;
    self.size <- 1 + self.size;
    push_unprotected self ~elems:xs

let push (self : _ t) elems : [ `Dropped | `Ok ] =
  protect_mutex self.mutex @@ fun () ->
  if self.size >= self.high_watermark then
    (* drop this to prevent queue from growing too fast *)
    `Dropped
  else (
    if self.size = 0 && Option.is_some self.timeout then
      (* current batch starts now *)
      self.start <- Mtime_clock.now ();

    (* add to queue *)
    push_unprotected self ~elems;
    `Ok
  )
