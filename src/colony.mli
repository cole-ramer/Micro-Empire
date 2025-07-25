open! Core

type t = {
  size : int;
  locations : Position.Set.t;
  energy : int;
  nutrient_absorption_level : Upgrades.Level.t;
  decay_reduction_level : Upgrades.Level.t;
  strength_level : Upgrades.Level.t;
  movement_level : Upgrades.Level.t;
}
[@@deriving sexp]

(* Takes in a colony, a game board, and a direction to move. 
Attempts to move all positions in the colony by one in the direction given.
Returns Some (colony with locations field set to the moved positions). 
If the colony remains in bounds or None if the move would cause one of 
the positions to go out of bounds *)
val move : t -> Board.t -> Dir.t -> t option

(* Takes in two colonies and handles the fight logic between them based on their size, strength level, and a bit of randomness.
 The winner colony will have an energey and size of the two colonies combined. Additonally the winning colony will grow cells 
 adjacent to itself equilivant to the number of overlapping cells between the two colonies (this perserves the combine size).
 The winning colony will be returned as Some {t with updated fields from fight}, and None for the colony that lost.
 The tuple ordering coresponds to the label given in function call -- (result of colony1, result of colony2)*)
val fight : colony1:t -> colony2:t -> Board.t -> t option * t option
