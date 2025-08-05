open! Core

module Enemy_target : sig
  type t
end

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : (int, Colony.t) Hashtbl.t;
  enemy_targets : (int, Enemy_target.t) Hashtbl.t;
  nutrients : Position.Hash_Set.t list;
  board : Board.t;
  creation_id_generator : Creation_id.t;
  time_of_last_move_of_enemies : (int, Time_ns.t) Hashtbl.t;
}

val handle_key : t -> char -> t option
val update_environment : t -> t
val create : width:int -> height:int -> t
val upgrade_board : t -> t
