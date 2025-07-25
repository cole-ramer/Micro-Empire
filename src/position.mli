open! Core

type t = { x : int; y : int } [@@deriving sexp, compare, equal]

include Comparable.S with type t := t

val position_right : t -> t
val position_left : t -> t
val position_up : t -> t
val position_down : t -> t
val adjacent_positions : t -> Set.t
