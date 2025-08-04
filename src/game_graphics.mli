open! Core

val init_exn : unit -> Game.t
val render : Game.t -> unit
val read_key : unit -> char option
val set_error : int -> unit
val fade_error_message : unit -> unit
val expand_visual : unit -> unit
val main_menu : unit -> unit
