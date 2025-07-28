open! Core

let choose ~(amount_to_choose : int) ~(amount_to_choose_from : int) =
  let choices = List.init amount_to_choose_from ~f:Fn.id in
  List.fold choices ~init:([], amount_to_choose) ~f:(fun (chosen_so_far, m) i ->
      let choices_left = amount_to_choose_from - i in
      let rand = Random.int_incl 1 choices_left in
      if Int.(rand <= m) then (i :: chosen_so_far, m - 1) else (chosen_so_far, m))
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

let rec expand_randomly (current_locations : Position.Set.t) (board : Board.t)
    ~size_to_increase =
  if size_to_increase = 0 then current_locations
  else
    let current_locations =
      increase_size current_locations board ~size_increase:1
    in
    expand_randomly current_locations board
      ~size_to_increase:(size_to_increase - 1)
