open! Core

module Level : sig
  type t = int [@@deriving sexp]
end

type t = Nutrient_absorption | Decary_reduction | Movement | Strength | Size
[@@deriving sexp]

val upgrade_cost : ?level:Level.t -> ?size:int -> t -> int
val upgrade_effect : ?level:Level.t -> ?size:int -> t -> int
