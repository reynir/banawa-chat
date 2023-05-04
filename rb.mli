(** A simple ring-buffer. *)

type ('c, 'a) t constraint 'c = < .. >
(** The type of a ring-buffer whose elements have type ['a]. *)

type ro = < rd : unit >
type wo = < wr : unit >
type rdwr = < rd : unit ; wr : unit >
type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a

val make : int -> (< rd : unit ; wr : unit >, 'a) t
val length : ('c rd, 'a) t -> int
val is_empty : ('c rd, 'a) t -> bool
val available : ('c rd, 'a) t -> int
val is_full : ('c rd, 'a) t -> bool
val push : ('c wr, 'a) t -> 'a -> unit
val pop : ('c wr, 'a) t -> 'a
val fit_and_push : ('c wr, 'a) t -> 'a -> unit
val drop : ('c wr, 'a) t -> unit
val iter : f:('a -> 'acc -> 'acc) -> ('c rd, 'a) t -> 'acc -> 'acc
val rev_iter : f:('a -> 'acc -> 'acc) -> ('c rd, 'a) t -> 'acc -> 'acc
val ( .%[] ) : ('c rd, 'a) t -> int -> 'a
val to_ro : ('c rd, 'a) t -> (ro, 'a) t
val to_wo : ('c wr, 'a) t -> (wo, 'a) t
