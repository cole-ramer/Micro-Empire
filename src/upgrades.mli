open! Core

module Level : sig
  type t = int [@@deriving sexp]
end

type t = Nutrient_absorption | Decary_reduction | Movement | Strength | Size
[@@deriving sexp]

(* Returns the cost for the given upgrade type depnding on potentially the level and size.
Here is a guide for when to include the paramaters.
Size increase -> only pass in size
everything else -> only pass in level*)
val upgrade_cost : ?level:Level.t -> ?size:int -> t -> int

(* Returns the effect of some of action based on potentially the size and level of the upgrade.
Here is a guide for what to pass in and what the value repersents:
| Nutrient_absorption -> pass in: level; result: energy gained
| Decay_reduction -> pass in: level and size; result: number of cells lost
| Strength -> pass in: level and size; result: the power of the colony for a fight
| Movement -> pass in: level and size; result: cost of the movement
| Size: -> pass in: size; result: number of cells added for manual size increase*)
val upgrade_effect : ?level:Level.t -> ?size:int -> t -> int
