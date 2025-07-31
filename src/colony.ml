open! Core

type t = {
  size : int;
  locations : Position.Set.t;
  energy : int;
  nutrient_absorption_level : Upgrades.Level.t;
  decay_reduction_level : Upgrades.Level.t;
  strength_level : Upgrades.Level.t;
  movement_level : Upgrades.Level.t;
  peak_size : int;
}
[@@deriving sexp]

(** Choose m numbers from 0 - n-1*)

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
  | Some new_locations -> (
      let movement_cost =
        Upgrades.upgrade_effect ~size:t.size ~level:t.movement_level
          Upgrades.Movement
      in
      let new_energy_total = t.energy - movement_cost in
      match new_energy_total >= 0 with
      | true ->
          Some
            {
              t with
              locations = new_locations;
              energy = new_energy_total - movement_cost;
            }
      | false -> None)
  | None -> Some t

let get_upgrade_cost colony (upgrade : Upgrades.t) =
  match upgrade with
  | Nutrient_absorption ->
      Upgrades.upgrade_cost ~level:colony.nutrient_absorption_level upgrade
  | Decay_reduction ->
      Upgrades.upgrade_cost ~level:colony.decay_reduction_level upgrade
  | Movement -> Upgrades.upgrade_cost ~level:colony.movement_level upgrade
  | Strength -> Upgrades.upgrade_cost ~level:colony.strength_level upgrade
  | Size -> Upgrades.upgrade_cost ~size:colony.size upgrade

let can_purchase_upgrade colony (upgrade : Upgrades.t) =
  colony.energy >= get_upgrade_cost colony upgrade

(* Custom option maybe*)
let upgrade ?(board : Board.t option) colony upgrade =
  match can_purchase_upgrade colony upgrade with
  | false -> None
  | true -> (
      let upgrade_cost = get_upgrade_cost colony upgrade in
      let new_energy = colony.energy - upgrade_cost in
      match upgrade with
      | Nutrient_absorption ->
          Some
            {
              colony with
              nutrient_absorption_level = colony.nutrient_absorption_level + 1;
              energy = new_energy;
            }
      | Decay_reduction ->
          Some
            {
              colony with
              decay_reduction_level = colony.decay_reduction_level + 1;
              energy = new_energy;
            }
      | Movement ->
          Some
            {
              colony with
              movement_level = colony.movement_level + 1;
              energy = new_energy;
            }
      | Strength ->
          Some
            {
              colony with
              strength_level = colony.strength_level + 1;
              energy = new_energy;
            }
      | Size -> (
          match board with
          | Some b ->
              let size_increase =
                Upgrades.upgrade_effect ~size:colony.size upgrade
              in
              let new_locations =
                Util.increase_size colony.locations b ~size_increase
              in
              Some
                {
                  colony with
                  locations = new_locations;
                  energy = new_energy;
                  size = colony.size + size_increase;
                  peak_size = max (colony.size + size_increase) colony.peak_size;
                }
          | None ->
              raise_s
                [%message
                  "purchased the increased size upgrade but did not pass in \
                   board as optional parameter"]))

let fight ~(colony1 : t) ~(colony2 : t) : t option * t option =
  let colony1_power =
    Upgrades.upgrade_effect ~size:colony1.size ~level:colony1.strength_level
      Upgrades.Strength
  in
  let colony2_power =
    Upgrades.upgrade_effect ~size:colony2.size ~level:colony2.strength_level
      Upgrades.Strength
  in
  let new_energy =
    (* print_s [%message (colony1 : t) (colony2 : t)]; *)
    colony1.energy + colony2.energy
  in
  let combined_locations = Set.union colony1.locations colony2.locations in
  let new_size = Set.length combined_locations in

  match colony1_power > colony2_power with
  | true ->
      ( Some
          {
            colony1 with
            energy = new_energy;
            size = new_size;
            locations = combined_locations;
            peak_size = max new_size colony1.peak_size;
          },
        None )
  | false ->
      ( None,
        Some
          {
            colony2 with
            energy = new_energy;
            size = new_size;
            locations = combined_locations;
            peak_size = max new_size colony2.peak_size;
          } )

let consume_nutrient colony =
  let energy_increase =
    Upgrades.upgrade_effect ~level:colony.nutrient_absorption_level
      Upgrades.Nutrient_absorption
  in
  { colony with energy = colony.energy + energy_increase }

let decay colony =
  let decay_amount =
    Upgrades.upgrade_effect ~level:colony.decay_reduction_level
      ~size:colony.size Upgrades.Decay_reduction
  in
  let new_locations =
    Util.shrink_randomly colony.locations ~size_decrease:decay_amount
  in
  let new_size = Set.length new_locations in
  { colony with locations = new_locations; size = new_size }

let create_empty_colony ?peak_size () =
  let peak_size = match peak_size with None -> 0 | Some size -> size in
  {
    energy = 0;
    size = 0;
    locations = Position.Set.empty;
    nutrient_absorption_level = 0;
    decay_reduction_level = 0;
    movement_level = 0;
    strength_level = 0;
    peak_size;
  }

let center t =
  if Set.length t.locations = 0 then { Position.x = 0; y = 0 }
  else
    let first = Option.value_exn (Set.nth t.locations 0) in
    let xmin, xmax, ymin, ymax =
      Set.fold t.locations ~init:(first.x, first.x, first.y, first.y)
        ~f:(fun (xmin, xmax, ymin, ymax) { Position.x; Position.y } ->
          (min xmin x, max xmax x, min ymin y, max ymax y))
    in
    { Position.x = (xmin + xmax) / 2; y = (ymin + ymax) / 2 }

let length t =
  if Set.length t.locations = 0 then 0
  else
    let first = Option.value_exn (Set.nth t.locations 0) in
    let xmin, xmax, ymin, ymax =
      Set.fold t.locations ~init:(first.x, first.x, first.y, first.y)
        ~f:(fun (xmin, xmax, ymin, ymax) { Position.x; Position.y } ->
          (min xmin x, max xmax x, min ymin y, max ymax y))
    in
    max (xmax - xmin) (ymax - ymin)

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
      peak_size = 0;
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
