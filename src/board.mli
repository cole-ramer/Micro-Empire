open! Core

type t = { height : int; width : int }

module Window : sig
  type t = { x_min : int; x_max : int; y_min : int; y_max : int }
  [@@deriving sexp]

  val is_in_window : t -> Position.t -> bool
end

val is_in_bounds : t -> Position.t -> bool
val create : height:int -> width:int -> t
