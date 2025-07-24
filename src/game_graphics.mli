open! Core

val init_exn : unit -> Game.t

val render : Game.t -> unit

val read_key : unit -> (char option)
