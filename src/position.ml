open! Core

module T = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]
end

include T
include Comparable.Make (T)

let position_right t = { x = t.x + 1; y = t.y }
let position_left t = { x = t.x - 1; y = t.y }
let position_up t = { x = t.x; y = t.y - 1 }
let position_down t = { x = t.x; y = t.y + 1 }

let adjacent_positions position =
  Set.of_list
    [
      position_right position;
      position_left position;
      position_down position;
      position_up position;
      position_left position |> position_up;
      position_right position |> position_up;
      position_left position |> position_down;
      position_right position |> position_down;
    ]
