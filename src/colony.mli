open! Core

type t = {
  size : int;
  locations : Position.Set.t;
  energy : int;
  nutrient_absorption_level : Upgrades.Level.t;
  decay_reduction_level : Upgrades.Level.t;
  strength_level : Upgrades.Level.t;
  movement_level : Upgrades.Level.t;
  peak_size : int;
}
[@@deriving sexp]

(* Takes in a colony, a game board, and a direction to move. 
Attempts to move all positions in the colony by one in the direction given.
Returns a colony with the updated positions. If the colony goes out of bounds 
it will return the colony unchanged.
NOTE: the move function does not handle the cost of moving the colony *)
val move : t -> Board.t -> Dir.t -> t

(* Takes in two colonies and handles the fight logic between them based on their size, strength level, and a bit of randomness.
 The winner colony will have an energey and size of the two colonies combined. Additonally the winning colony will grow cells 
 adjacent to itself equilivant to the number of overlapping cells between the two colonies (this perserves the combine size).
 The winning colony will be returned as Some {t with updated fields from fight}, and None for the colony that lost.
 The tuple ordering coresponds to the label given in function call -- (result of colony1, result of colony2)*)
val fight : colony1:t -> colony2:t -> t option * t option

(* Takes in a colony and upgrade and returns whether 
the next level for said upgrade type can be purchased*)
val can_purchase_upgrade : t -> Upgrades.t -> bool

(*Takes in a colony and upgrade type and potentially a board.
If the colony can not afford the upgrade it will return None.
Otherwise it will return Some (colony with updated energy and upgrade level).
NOTE FOR SIZE: size is the only upgrade that requires the Board.t
Additonally the colony will also have an updated location set for the new cells *)
val upgrade : ?board:Board.t -> t -> Upgrades.t -> t option

(* Creates a colony with an empty location set, 0 energy, size ,nutrient_absorption_level, etc*)
val create_empty_colony : ?peak_size:int -> unit -> t

(* Returns a colony with increased energy level from consuming the nutrient *)
val consume_nutrient : t -> t

(* Decays the colony, randomly removing a number of cells (will often be 0)
influnced by the size and decay reduction level of the colony*)
val decay : t -> t

(* Gets position that is in the center of the colonies extremas.
i.e. center = (x_max + x_min) / 2, (y_max + y_min) / 2*)
val center : t -> Position.t
val length : t -> int

(* returns a list of upgrades the colony can afford *)
val possible_upgrades : t -> Upgrades.t list
