open! Core

module Level : sig
  type t = int [@@deriving sexp]
end

type t = Nutrient_absorption | Decay_reduction | Movement | Strength
[@@deriving sexp]

(* Returns the cost for the given upgrade type depnding on the level.*)
val upgrade_cost : level:Level.t -> t -> int

(* Returns the effect of some of action based on the size and level of the upgrade.
Here is a guide for what to pass in and what the value repersents:
| Nutrient_absorption -> pass in: level; result: energy gained
| Decay_reduction -> pass in: level and size; result: number of cells lost
| Strength -> pass in: level and size; result: the power of the colony for a fight
| Movement -> pass in: level and size; result: cost of the movement*)
val upgrade_effect : level:Level.t -> ?size:int -> t -> int
