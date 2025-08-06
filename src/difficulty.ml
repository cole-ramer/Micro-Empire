open! Core

type t = Easy | Medium | Hard [@@deriving sexp]

let directions_list = [ Easy; Medium; Hard ]

let to_string t =
  match t with Easy -> "Easy" | Medium -> "Medium" | Hard -> "Hard"

let next t = match t with Easy -> Medium | Medium -> Hard | Hard -> Easy
