open! Core

type t = { height : int; width : int }

module Window = struct
  type t = { x_min : int; x_max : int; y_min : int; y_max : int }
  [@@deriving sexp]

  let is_in_window window (position : Position.t) =
    position.x >= window.x_min && position.x <= window.x_max
    && position.y >= window.y_min && position.y <= window.y_max
end

let is_in_bounds t (position : Position.t) =
  position.y >= 0 && position.y < t.height && position.x >= 0
  && position.x < t.width

let create ~(height : int) ~(width : int) : t = { height; width }
