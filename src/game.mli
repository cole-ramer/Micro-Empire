open! Core

module Enemy_target : sig
  type t
end

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : (int, Colony.t) Hashtbl.t;
  enemy_targets : (int, Enemy_target.t) Hashtbl.t;
  board : Board.t;
  creation_id_generator : Creation_id.t;
  nutrient_position_id_map : (Position.t, int) Hashtbl.t;
  nutrient_id_cluster_map : (int, Position.Hash_Set.t) Hashtbl.t;
  time_of_last_move_of_enemies : (int, Time_ns.t) Hashtbl.t;
  difficulty : Difficulty.t;
}

val handle_key : t -> char -> t option
val update_environment : t -> t
val create : width:int -> height:int -> difficulty:Difficulty.t -> t
val upgrade_board : t -> t
