open! Core

module Enemy_target : sig
  type t
end

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : (int, Colony.t) Hashtbl.t;
  enemy_targets : (int, Enemy_target.t) Hashtbl.t;
  mutable board : Board.t;
  creation_id_generator : Creation_id.t;
  nutrient_position_id_map : (Position.t, int) Hashtbl.t;
  nutrient_id_cluster_map : (int, Position.Hash_Set.t) Hashtbl.t;
  time_of_last_move_of_enemies : (int, Time_ns.t) Hashtbl.t;
  difficulty : Difficulty.t;
}

(* Handles player input, Only returns None if the player attempts to
  purchase and upgrade they can not afford. Otherwise returns Some new_game.
The bool in the tuple is whether or not the game was updated from this key press *)
val handle_key : t -> char -> t option * bool

(* returns the game with the nutrients consumption, fight, and enemy movment, effects
applied to it, along with bool on whether the game was updated this call*)
val update_environment : t -> t * bool

(* Creates the inital board *)
val create : width:int -> height:int -> difficulty:Difficulty.t -> t

(* Increase the games board size in place*)
val upgrade_board : t -> unit
