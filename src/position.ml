open! Core

module T = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, equal, hash]
end

include T
include Comparable.Make (T)
module Hash_Set = Hash_set.Make (T)
module Table = Hashtbl.Make (T)

let position_right t = { x = t.x + 1; y = t.y }
let position_left t = { x = t.x - 1; y = t.y }
let position_up t = { x = t.x; y = t.y + 1 }
let position_down t = { x = t.x; y = t.y - 1 }

let get_distance pos1 pos2 =
  Int.abs (pos1.x - pos2.x) + Int.abs (pos1.y - pos2.y)

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
