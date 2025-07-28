open! Core

type t = { mutable id : int }

let next_id t =
  t.id <- t.id + 1;
  t.id

let create () = { id = 0 }
