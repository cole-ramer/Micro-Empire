open! Core
type t = {
  player : Colony.t
  ; game_state : Game_state.t
  ; enemies : Colony.t list 
  ; nutrients : Position.t list
  ; board : Board.t
}