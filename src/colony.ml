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

(** Choose m numbers from 0 - n-1*)
let choose ~amount_to_choose ~amount_to_choose_from =
  let choices = List.init amount_to_choose_from ~f:Fn.id in
  List.fold choices ~init:([], amount_to_choose) ~f:(fun (chosen_so_far, m) i ->
      let choices_left = amount_to_choose_from - i in
      let rand = Random.int_incl 1 choices_left in
      if rand <= m then (i :: chosen_so_far, m - 1) else (chosen_so_far, m))
  |> fst |> Int.Set.of_list

let%expect_test "choose function" =
  List.init 20 ~f:(fun _ -> ())
  |> List.iter ~f:(fun () ->
         print_s
           [%message
             (choose ~amount_to_choose:2 ~amount_to_choose_from:6 : Int.Set.t)]);
  [%expect
    {|
    ("choose 2 6" (2 0))
    ("choose 2 6" (3 0))
    ("choose 2 6" (1 0))
    ("choose 2 6" (2 1))
    ("choose 2 6" (4 1))
    ("choose 2 6" (5 1))
    ("choose 2 6" (3 2))
    ("choose 2 6" (3 2))
    ("choose 2 6" (4 1))
    ("choose 2 6" (5 1))
    ("choose 2 6" (4 3))
    ("choose 2 6" (5 3))
    ("choose 2 6" (3 0))
    ("choose 2 6" (4 0))
    ("choose 2 6" (3 1))
    ("choose 2 6" (4 3))
    ("choose 2 6" (5 1))
    ("choose 2 6" (3 0))
    ("choose 2 6" (5 4))
    ("choose 2 6" (1 0)) |}]

let increase_size (colony_locations : Position.Set.t) (board : Board.t)
    ~size_increase =
  let availble_positions =
    Set.to_list colony_locations
    |> List.map ~f:(fun colony_position ->
           Position.adjacent_positions colony_position)
    |> Position.Set.union_list
  in
  let availble_positions =
    Set.filter availble_positions ~f:(fun possible_position ->
        (not (Set.mem colony_locations possible_position))
        && Board.is_in_bounds board possible_position)
    |> Set.to_list
  in
  let new_colony_positions_indexes =
    choose ~amount_to_choose:size_increase
      ~amount_to_choose_from:(List.length availble_positions)
  in
  let picked_positions =
    List.filteri availble_positions ~f:(fun index _ ->
        Set.mem new_colony_positions_indexes index)
    |> Position.Set.of_list
  in
  Set.union picked_positions colony_locations

let fight ~colony1 ~colony2 (board : Board.t) : t option * t option =
  let colony1_power =
    Upgrades.upgrade_effect ~size:colony1.size ~level:colony1.strength_level
      Upgrades.Strength
  in
  let colony2_power =
    Upgrades.upgrade_effect ~size:colony2.size ~level:colony2.strength_level
      Upgrades.Strength
  in
  let new_energy = colony1.energy + colony2.energy in
  let new_size = colony1.size + colony2.size in
  let combined_locations = Set.union colony1.locations colony2.locations in
  let new_locations =
    increase_size combined_locations board
      ~size_increase:(new_size - Set.length combined_locations)
  in
  match colony1_power > colony2_power with
  | true ->
      ( Some
          {
            colony1 with
            energy = new_energy;
            size = new_size;
            locations = new_locations;
          },
        None )
  | false ->
      ( None,
        Some
          {
            colony2 with
            energy = new_energy;
            size = new_size;
            locations = new_locations;
          } )

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
