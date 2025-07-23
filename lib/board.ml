open! Core

type t = { height : int; width : int }

let is_in_bounds t (position : Position.t) =
  position.y >= 0 && position.y < t.height && position.x >= 0
  && position.x < t.width

let create ~(height : int) ~(width : int) : t = { height; width }
