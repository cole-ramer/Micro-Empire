open! Core

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : Colony.t list;
  nutrients : Position.Set.t;
  board : Board.t;
}

val handle_key : t -> char -> t
val update_environment : t -> t
val create : width:int -> height:int -> t
