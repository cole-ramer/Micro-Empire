open! Core

type t = Easy | Medium | Hard [@@deriving sexp]

val directions_list : t list

val to_string : t -> string

val next : t -> t