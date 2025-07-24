open! Core

val init_exn : unit -> unit
(*should be unit -> game*)

val render : Game.t -> unit

val read_key : unit -> (char option)
