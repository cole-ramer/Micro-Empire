open! Core

type t = { height : int; width : int }

val is_in_bounds : t -> Position.t -> bool
val create : height:int -> width:int -> t
