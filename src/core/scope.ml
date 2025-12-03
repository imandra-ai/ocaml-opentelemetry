open Common_

type item_list =
  | Nil
  | Ev of Event.t * item_list
  | Attr of Key_value.t * item_list
  | Span_link of Span_link.t * item_list
  | Span_status of Span_status.t * item_list
  | Span_kind of Span_kind.t * item_list

type t = {
  trace_id: Trace_id.t;
  span_id: Span_id.t;
  mutable items: item_list;
}

let attrs scope =
  let rec loop acc = function
    | Nil -> acc
    | Attr (attr, l) -> loop (attr :: acc) l
    | Ev (_, l) | Span_kind (_, l) | Span_link (_, l) | Span_status (_, l) ->
      loop acc l
  in
  loop [] scope.items

let events scope =
  let rec loop acc = function
    | Nil -> acc
    | Ev (event, l) -> loop (event :: acc) l
    | Attr (_, l) | Span_kind (_, l) | Span_link (_, l) | Span_status (_, l) ->
      loop acc l
  in
  loop [] scope.items

let links scope =
  let rec loop acc = function
    | Nil -> acc
    | Span_link (span_link, l) -> loop (span_link :: acc) l
    | Ev (_, l) | Span_kind (_, l) | Attr (_, l) | Span_status (_, l) ->
      loop acc l
  in
  loop [] scope.items

let status scope =
  let rec loop = function
    | Nil -> None
    | Span_status (status, _) -> Some status
    | Ev (_, l) | Attr (_, l) | Span_kind (_, l) | Span_link (_, l) -> loop l
  in
  loop scope.items

let kind scope =
  let rec loop = function
    | Nil -> None
    | Span_kind (k, _) -> Some k
    | Ev (_, l) | Span_status (_, l) | Attr (_, l) | Span_link (_, l) -> loop l
  in
  loop scope.items

let make ~trace_id ~span_id ?(events = []) ?(attrs = []) ?(links = []) ?status
    () : t =
  let items =
    let items =
      match status with
      | None -> Nil
      | Some status -> Span_status (status, Nil)
    in
    let items = List.fold_left (fun acc ev -> Ev (ev, acc)) items events in
    let items = List.fold_left (fun acc attr -> Attr (attr, acc)) items attrs in
    List.fold_left (fun acc link -> Span_link (link, acc)) items links
  in
  { trace_id; span_id; items }

let[@inline] to_span_link ?trace_state ?attrs ?dropped_attributes_count
    (self : t) : Span_link.t =
  Span_link.make ?trace_state ?attrs ?dropped_attributes_count
    ~trace_id:self.trace_id ~span_id:self.span_id ()

let[@inline] to_span_ctx (self : t) : Span_ctx.t =
  Span_ctx.make ~trace_id:self.trace_id ~parent_id:self.span_id ()

open struct
  let[@inline] is_not_dummy (self : t) : bool = Span_id.is_valid self.span_id
end

let[@inline] add_event (self : t) (ev : unit -> Event.t) : unit =
  if is_not_dummy self then self.items <- Ev (ev (), self.items)

let[@inline] record_exception (self : t) (exn : exn)
    (bt : Printexc.raw_backtrace) : unit =
  if is_not_dummy self then (
    let ev =
      Event.make "exception"
        ~attrs:
          [
            "exception.message", `String (Printexc.to_string exn);
            "exception.type", `String (Printexc.exn_slot_name exn);
            ( "exception.stacktrace",
              `String (Printexc.raw_backtrace_to_string bt) );
          ]
    in
    self.items <- Ev (ev, self.items)
  )

let[@inline] add_attrs (self : t) (attrs : unit -> Key_value.t list) : unit =
  if is_not_dummy self then
    self.items <-
      List.fold_left (fun acc attr -> Attr (attr, acc)) self.items (attrs ())

let[@inline] add_links (self : t) (links : unit -> Span_link.t list) : unit =
  if is_not_dummy self then
    self.items <-
      List.fold_left
        (fun acc link -> Span_link (link, acc))
        self.items (links ())

let set_status (self : t) (status : Span_status.t) : unit =
  if is_not_dummy self then self.items <- Span_status (status, self.items)

let set_kind (self : t) (k : Span_kind.t) : unit =
  if is_not_dummy self then self.items <- Span_kind (k, self.items)

let ambient_scope_key : t Ambient_context.key = Ambient_context.create_key ()

let get_ambient_scope ?scope () : t option =
  match scope with
  | Some _ -> scope
  | None -> Ambient_context.get ambient_scope_key

let[@inline] with_ambient_scope (sc : t) (f : unit -> 'a) : 'a =
  Ambient_context.with_binding ambient_scope_key sc (fun _ -> f ())
