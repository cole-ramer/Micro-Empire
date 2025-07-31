open! Core

type t = Right | Left | Up | Down [@@deriving sexp]

val directions_list : t list

val possible_dir_to_reach_target :
  source:Position.t -> target:Position.t -> t list
