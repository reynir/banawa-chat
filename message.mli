type t

val split_at : len:int -> t -> string list
val make : nickname:string -> string -> t
val nickname : t -> string
val message : t -> string

val msgf :
  ?nickname:string
  -> ('a, Format.formatter, unit, t) format4
  -> 'a
