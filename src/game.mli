open! Core

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : Colony.t Int.Map.t;
  nutrients : Position.Set.t list;
  board : Board.t;
  creation_id_generator : Creation_id.t;
}

val handle_key : t -> char -> t option
val update_environment : t -> t
val create : width:int -> height:int -> t
