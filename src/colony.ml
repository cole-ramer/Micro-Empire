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
  let new_locations_option =
    Set.fold_until t.locations ~init:Position.Set.empty
      ~f:(fun
          (new_locations_set : Position.Set.t)
          (current_location : Position.t)
        ->
        let possible_new_position = move_function current_location in
        match Board.is_in_bounds board possible_new_position with
        | true -> Continue (Set.add new_locations_set possible_new_position)
        | false -> Stop None)
      ~finish:(fun new_locations_set -> Some new_locations_set)
  in
  match new_locations_option with
  | Some new_locations ->
      let movement_cost =
        Upgrades.upgrade_effect ~size:t.size ~level:t.movement_level
          Upgrades.Movement
      in
      let new_energy_total = t.energy - movement_cost in
      Some
        {
          t with
          locations = new_locations;
          energy = new_energy_total - movement_cost;
        }
  | None -> None

(*-------------------- Testing ------------------*)
let four_by_four = Board.create ~height:4 ~width:4
let four_by_three = Board.create ~height:3 ~width:4
let empty_board = Board.create ~height:0 ~width:0

let print_moved_colonies ~(original_positions : Position.Set.t)
    ~(board : Board.t) =
  let (orignal_colony : t) =
    {
      size = 0;
      locations = original_positions;
      energy = 0;
      nutrient_absorption_level = 0;
      decay_reduction_level = 0;
      strength_level = 0;
      movement_level = 0;
    }
  in
  let colony_right = move orignal_colony board Dir.Right in
  (match colony_right with
  | Some moved_colony ->
      print_s
        [%message "Colony right" (moved_colony.locations : Position.Set.t)]
  | None -> ());
  let colony_left = move orignal_colony board Dir.Left in
  (match colony_left with
  | Some moved_colony ->
      print_s [%message "Colony left" (moved_colony.locations : Position.Set.t)]
  | None -> ());
  let colony_down = move orignal_colony board Dir.Down in
  (match colony_down with
  | Some moved_colony ->
      print_s [%message "Colony down" (moved_colony.locations : Position.Set.t)]
  | None -> ());
  let colony_up = move orignal_colony board Dir.Up in
  match colony_up with
  | Some moved_colony ->
      print_s [%message "Colony up" (moved_colony.locations : Position.Set.t)]
  | None -> ()

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
  print_moved_colonies ~original_positions:positions ~board;

  [%expect
    {|
    ("Colony right"
     (moved_colony.locations
      (((x 1) (y 1)) ((x 1) (y 2)) ((x 2) (y 0)) ((x 2) (y 1)) ((x 2) (y 2))
       ((x 2) (y 3)) ((x 3) (y 1)) ((x 3) (y 2))))) |}]

let%expect_test "plus sign only able to move left" =
  let board = four_by_four in
  let original_positions =
    Position.Set.of_list
      [
        { x = 2; y = 0 };
        { x = 1; y = 1 };
        { x = 2; y = 1 };
        { x = 3; y = 1 };
        { x = 1; y = 2 };
        { x = 2; y = 2 };
        { x = 3; y = 2 };
        { x = 2; y = 3 };
      ]
  in
  print_moved_colonies ~original_positions ~board;
  [%expect
    {|
("Colony left"
 (moved_colony.locations
  (((x 0) (y 1)) ((x 0) (y 2)) ((x 1) (y 0)) ((x 1) (y 1)) ((x 1) (y 2))
   ((x 1) (y 3)) ((x 2) (y 1)) ((x 2) (y 2)))))
|}]

let%expect_test "empty set non-empty board" =
  let board = four_by_four in
  let original_positions = Position.Set.empty in
  print_moved_colonies ~original_positions ~board;
  [%expect
    {|
    ("Colony right" (moved_colony.locations ()))
    ("Colony left" (moved_colony.locations ()))
    ("Colony down" (moved_colony.locations ()))
    ("Colony up" (moved_colony.locations ())) |}]

let%expect_test "empty set empty board" =
  let board = empty_board in
  let original_positions = Position.Set.empty in
  print_moved_colonies ~original_positions ~board;
  [%expect
    {|
    ("Colony right" (moved_colony.locations ()))
    ("Colony left" (moved_colony.locations ()))
    ("Colony down" (moved_colony.locations ()))
    ("Colony up" (moved_colony.locations ())) |}]

let%expect_test "non empty set empty board" =
  let board = empty_board in
  let original_positions =
    Position.Set.of_list
      [
        { x = 2; y = 0 };
        { x = 1; y = 1 };
        { x = 2; y = 1 };
        { x = 3; y = 1 };
        { x = 1; y = 2 };
        { x = 2; y = 2 };
        { x = 3; y = 2 };
        { x = 2; y = 3 };
      ]
  in
  print_moved_colonies ~original_positions ~board;
  [%expect {||}]

let%expect_test "colony only able to move down" =
  let board = four_by_four in
  let original_positions =
    Position.Set.of_list
      [
        { x = 0; y = 0 };
        { x = 1; y = 0 };
        { x = 2; y = 0 };
        { x = 3; y = 0 };
        { x = 1; y = 1 };
        { x = 2; y = 1 };
      ]
  in
  print_moved_colonies ~original_positions ~board;

  [%expect
    {|
("Colony down"
 (moved_colony.locations
  (((x 0) (y 1)) ((x 1) (y 1)) ((x 1) (y 2)) ((x 2) (y 1)) ((x 2) (y 2))
   ((x 3) (y 1)))))
|}]

let%expect_test "colony only able to move up" =
  let board = four_by_four in
  let original_positions =
    Position.Set.of_list
      [
        { x = 1; y = 2 };
        { x = 2; y = 2 };
        { x = 0; y = 3 };
        { x = 1; y = 3 };
        { x = 2; y = 3 };
        { x = 3; y = 3 };
      ]
  in
  print_moved_colonies ~original_positions ~board;

  [%expect
    {|
("Colony up"
 (moved_colony.locations
  (((x 0) (y 2)) ((x 1) (y 1)) ((x 1) (y 2)) ((x 2) (y 1)) ((x 2) (y 2))
   ((x 3) (y 2)))))
|}]
