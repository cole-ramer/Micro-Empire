open! Core

let print_function_time (message : string) =
  let time =
    Time_ns.to_string_abs (Time_ns.now ())
      ~zone:(Core_private.Time_zone.of_utc_offset ~hours:(-4))
  in
  print_s [%message message time]

let print_time_diff (message : string) (start_time : Time_ns.t) =
  let end_time = Time_ns.now () in
  let time_diff =
    Time_ns.diff end_time start_time |> Time_ns.Span.to_short_string
  in
  print_s [%message message time_diff]

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

let increase_size (colony_locations : Position.Set.t)
    (currently_filled : Position.Set.t) (board : Board.t) ~size_increase =
  let available_positions =
    Set.diff
      (Set.to_list colony_locations
      |> List.map ~f:(fun colony_position ->
             Position.adjacent_positions colony_position)
      |> Position.Set.union_list)
      currently_filled
  in
  let overlap pos = Set.mem colony_locations pos in
  let available_positions =
    Set.filter available_positions ~f:(fun possible_position ->
        (not (overlap possible_position))
        && Board.is_in_bounds board possible_position
        (* 
        && Set.fold (Position.adjacent_positions possible_position) ~init:true
             ~f:(fun status adj -> if status then overlap adj else false) *))
    |> Set.to_list
  in
  let new_colony_positions_indexes =
    choose ~amount_to_choose:size_increase
      ~amount_to_choose_from:(List.length available_positions)
  in
  let picked_positions =
    List.filteri available_positions ~f:(fun index _ ->
        Set.mem new_colony_positions_indexes index)
    |> Position.Set.of_list
  in
  Set.union picked_positions colony_locations

let rec expand_randomly (current_locations : Position.Set.t)
    (filled_positions : Position.Set.t) (board : Board.t) ~size_increase =
  if size_increase = 0 then current_locations
  else
    let current_locations =
      increase_size current_locations filled_positions board ~size_increase:1
    in
    expand_randomly current_locations filled_positions board
      ~size_increase:(size_increase - 1)

(* let rec dfs (starting_position : Position.t) ~(all_positions : Position.Set.t)
    ~(marked_positions : Position.Set.t) =
  let marked_positions = Set.add marked_positions starting_position in
  Set.fold (Position.adjacent_positions starting_position)
    ~init:marked_positions ~f:(fun currently_marked possible_position ->
      match
        ( Set.mem all_positions possible_position,
          Set.mem currently_marked possible_position )
      with
      | true, false ->
          dfs possible_position ~all_positions
            ~marked_positions:currently_marked
      | true, true | false, false -> currently_marked
      | false, true ->
          raise_s [%message "Marked a position not in the all positions set"]) *)
let dfs (all_positions : Position.Set.t) start_position =
  let visited = Position.Hash_Set.create () in
  let to_visit = Stack.create () in
  Stack.push to_visit start_position;
  let rec traverse () =
    match Stack.pop to_visit with
    | None -> ()
    | Some current_position ->
        if not (Hash_set.mem visited current_position) then (
          Hash_set.add visited current_position;
          Set.iter (Position.adjacent_positions current_position)
            ~f:(fun next_position ->
              match
                ( Set.mem all_positions next_position,
                  Hash_set.mem visited next_position )
              with
              | true, false -> Stack.push to_visit next_position
              | false, false | true, true -> ()
              | false, true ->
                  raise_s
                    [%message
                      "can not have visited a node not in the all positions set"]));
        traverse ()
  in
  traverse ();
  visited

(* let decrease_size (colony_locations : Position.Set.t) =
  match Set.length colony_locations = 1 with
  | true -> Position.Set.empty
  | false -> (
      let possilbe_positions =
        Set.filter colony_locations ~f:(fun position_in_colony ->
            let colony_without_the_position =
              Set.remove colony_locations position_in_colony
            in
            let starting_position =
              match Set.choose colony_without_the_position with
              | None ->
                  raise_s
                    [%message
                      (colony_without_the_position : Position.Set.t)
                        "has length not equal to one but was in the attempt \
                         dfs call"]
              | Some position -> position
            in
            let vistable = dfs colony_without_the_position starting_position in
            match Hash_set.length vistable = Set.length colony_without_the_position with
            |
            Set.fold colony_without_the_position)
      in
      match Set.is_empty possilbe_positions with
      | true -> colony_locations
      | false ->
          let random_index = Random.int (Set.length possilbe_positions) in
          let removed_position =
            match Set.nth possilbe_positions random_index with
            | None ->
                raise_s
                  [%message
                    (random_index : int)
                      "is not in"
                      (possilbe_positions : Position.Set.t)]
            | Some position -> position
          in
          Set.remove colony_locations removed_position) *)

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let rec shrink_randomly (colony_locations : Position.Set.t) ~size_decrease =
  (* if size_decrease = 0 then colony_locations
  else
    let colony_locations = decrease_size colony_locations in
    shrink_randomly colony_locations ~size_decrease:(size_decrease - 1) *)
  let start_time = Time_ns.now () in

  let shuffled_position_array = knuth_shuffle (Set.to_array colony_locations) in
  let final_locations =
    Array.fold_until shuffled_position_array
      ~init:(colony_locations, size_decrease)
      ~f:(fun (current_positions_left, left_to_remove) possible_position ->
        match left_to_remove > 0 with
        | false -> Stop current_positions_left
        | true -> (
            let set_with_pos_removed =
              Set.remove current_positions_left possible_position
            in
            match Set.choose set_with_pos_removed with
            | Some starting_position -> (
                let vistable = dfs set_with_pos_removed starting_position in
                match
                  Set.length set_with_pos_removed = Hash_set.length vistable
                with
                | true -> Continue (set_with_pos_removed, left_to_remove - 1)
                | false -> Continue (current_positions_left, left_to_remove))
            | None -> (
                match Set.length current_positions_left = 1 with
                | true -> Stop Position.Set.empty
                | false ->
                    raise_s
                      [%message
                        "was unable to choose from a set that was not of \
                         length 1"])))
      ~finish:(fun (positions_left, _) -> positions_left)
  in
  print_time_diff "decay fold" start_time;
  final_locations
