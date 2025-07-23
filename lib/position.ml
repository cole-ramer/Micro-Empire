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
