open! Core

type t = {
  size : int;
  locations : Position.Set.t;
  energy : int;
  nutrient_absorption_level : Upgrades.Level.t;
  decay_reduction_level : Upgrades.Level.t;
  strength_level : Upgrades.Level.t;
  movement_level : Upgrades.Level.t;
}
[@@deriving sexp]

let move t (board : Board.t) (direction : Dir.t) : t option =
  let move_function =
    match direction with
    | Right -> Position.position_right
    | Left -> Position.position_left
    | Down -> Position.position_down
    | Up -> Position.position_up
  in
  Set.fold_until t.locations ~init:Position.Set.empty
    ~f:(fun
        (new_locations_set : Position.Set.t) (current_location : Position.t) ->
      match Board.is_in_bounds board current_location with
      | true ->
          Continue (Set.add new_locations_set (move_function current_location))
      | false -> Stop None)
    ~finish:(fun new_locations_set ->
      Some { t with locations = new_locations_set })

let four_by_four = Board.create ~height:4 ~width:4
let four_by_three = Board.create ~height:3 ~width:4
let empty_board = Board.create ~height:0 ~width:0

let%expect_test "plus sign only able to move right" =
  let board = four_by_four in
  let positions =
    Position.Set.of_list
      [
        { x = 1; y = 0 };
        { x = 0; y = 1 };
        { x = 1; y = 1 };
        { x = 2; y = 1 };
        { x = 0; y = 2 };
        { x = 1; y = 2 };
        { x = 2; y = 2 };
        { x = 1; y = 3 };
      ]
  in
  let (orignal_colony : t) =
    {
      size = 0;
      locations = positions;
      energy = 0;
      nutrient_absorption_level = 0;
      decay_reduction_level = 0;
      strength_level = 0;
      movement_level = 0;
    }
  in
  let colony_right = move orignal_colony four_by_four Dir.Right in
  let colony_left = move orignal_colony four_by_four Dir.Left in
  let colony_down = move orignal_colony four_by_four Dir.Down in
  let colony_up = move orignal_colony four_by_four Dir.Up in
  print_s
    [%message
      (colony_right : t option)
        (colony_left : t option)
        (colony_down : t option)
        (colony_up : t option)]
