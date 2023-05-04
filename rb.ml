type ('c, 'a) t =
  { arr : 'a option array; mutable rd_cursor : int; mutable wr_cursor : int }
  constraint 'c = < .. >

type ro = < rd : unit >
type wo = < wr : unit >
type rdwr = < rd : unit ; wr : unit >
type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a

let make len = { arr = Array.make len None; rd_cursor = 0; wr_cursor = 0 }

exception Full
exception Empty

let length t =
  if t.rd_cursor <= t.wr_cursor then t.wr_cursor - t.rd_cursor
  else
    let len = Array.length t.arr in
    len - t.rd_cursor + t.wr_cursor

let is_empty t = length t = 0
let available t = Array.length t.arr - length t
let is_full t = length t = Array.length t.arr
let mask t v = v mod Array.length t.arr

let push t v =
  if is_full t then raise Full;
  t.arr.(t.wr_cursor) <- Some v;
  t.wr_cursor <- mask t (t.wr_cursor + 1)

let pop t =
  if is_empty t then raise Empty;
  let[@warning "-8"] (Some v) = t.arr.(t.rd_cursor) in
  t.rd_cursor <- mask t (t.rd_cursor + 1);
  v

let fit_and_push t v =
  if is_full t then ignore (pop t);
  push t v

let drop t =
  if is_empty t then raise Empty;
  t.wr_cursor <- mask t (t.wr_cursor - 1)

let iter ~f t a =
  let i = ref t.rd_cursor in
  let a = ref a in
  while !i <> t.wr_cursor do
    a := f (Option.get t.arr.(mask t !i)) !a;
    incr i
  done;
  !a

let rev_iter ~f t a =
  let i = ref (t.wr_cursor - 1) in
  let a = ref a in
  while !i >= t.rd_cursor do
    a := f (Option.get t.arr.(mask t !i)) !a;
    decr i
  done;
  !a

let ( .%[] ) t idx =
  if idx >= length t then invalid_arg "Out of bounds";
  Option.get t.arr.(mask t (t.rd_cursor + idx))

external to_ro : ('c rd, 'a) t -> (ro, 'a) t = "%identity"
external to_wo : ('c wr, 'a) t -> (wo, 'a) t = "%identity"
