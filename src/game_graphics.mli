open! Core

val init_exn : Difficulty.t -> Game.t
val render : Game.t -> unit
val read_key : unit -> char option
val set_error : int -> unit
val fade_error_message : unit -> unit
val expand_visual : unit -> unit
val main_menu : Difficulty.t -> unit
val close : unit -> unit
