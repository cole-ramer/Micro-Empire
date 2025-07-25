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
Returns Some (colony with locations field set to the moved positions,
 and energy field updated with the cost of the movement). 
If the colony remains in bounds or None if the move would cause one of 
the positions to go out of bounds.
NOTE: move does allow for the energy field to go or remain negative
because of the cost of the move. The caller should handle such case *)
val move : t -> Board.t -> Dir.t -> t option

(* Takes in two colonies and handles the fight logic between them based on their size, strength level, and a bit of randomness.
 The winner colony will have an energey and size of the two colonies combined. Additonally the winning colony will grow cells 
 adjacent to itself equilivant to the number of overlapping cells between the two colonies (this perserves the combine size).
 The winning colony will be returned as Some {t with updated fields from fight}, and None for the colony that lost.
 The tuple ordering coresponds to the label given in function call -- (result of colony1, result of colony2)*)
val fight : colony1:t -> colony2:t -> Board.t -> t option * t option

(* Takes in a colony and upgrade and returns whether 
the next level for said upgrade type can be purchased*)
val can_purchase_upgrade : t -> Upgrades.t -> bool

(*Takes in a colony and upgrade type and potentially a board.
If the colony can not afford the upgrade it will return None.
Otherwise it will return Some (colony with updated energy and upgrade level).
NOTE FOR SIZE: size is the only upgrade that requires the Board.t
Additonally the colony will also have an updated location set for the new cells *)
val upgrade : ?board:Board.t -> t -> Upgrades.t -> t option
