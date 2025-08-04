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

let get_random_position_from_hash_set (position_hash_set : Position.Hash_Set.t)
    =
  let random_index = Random.int (Hash_set.length position_hash_set) in

  Hash_set.fold_until position_hash_set ~init:0
    ~f:(fun current_index pos_to_add ->
      match current_index = random_index with
      | true -> Stop pos_to_add
      | false -> Continue (current_index + 1))
    ~finish:(fun _ ->
      raise_s [%message "Should not have gottent to finish for adding position"])

let expand_randomly ~(current_colony_locations : Position.Hash_Set.t)
    ~(all_game_filled_positions : Position.Hash_Set.t) (board : Board.t)
    ~size_increase =
  let new_positions = current_colony_locations in
  let available_positions = Position.Hash_Set.create () in
  Hash_set.iter current_colony_locations ~f:(fun colony_position ->
      Set.iter (Position.adjacent_positions colony_position)
        ~f:(fun adjacent_position ->
          match
            (not (Hash_set.mem all_game_filled_positions adjacent_position))
            && Board.is_in_bounds board adjacent_position
          with
          | true -> Hash_set.add available_positions adjacent_position
          | false -> ()));
  List.init size_increase ~f:Fn.id
  |> List.iter ~f:(fun _ ->
         let pos_to_add =
           get_random_position_from_hash_set available_positions
         in
         Hash_set.add new_positions pos_to_add;
         Set.iter (Position.adjacent_positions pos_to_add)
           ~f:(fun adjacent_position ->
             match
               (not (Hash_set.mem all_game_filled_positions adjacent_position))
               && Board.is_in_bounds board adjacent_position
             with
             | true -> Hash_set.add available_positions adjacent_position
             | false -> ()))

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
let dfs (all_positions : Position.Hash_Set.t) start_position =
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
                ( Hash_set.mem all_positions next_position,
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

let shrink_randomly (colony_locations : Position.Hash_Set.t) ~size_decrease =
  (* if size_decrease = 0 then colony_locations
  else
    let colony_locations = decrease_size colony_locations in
    shrink_randomly colony_locations ~size_decrease:(size_decrease - 1) *)
  let start_time = Time_ns.now () in
  match size_decrease = 0 with
  | true -> colony_locations
  | false ->
      let positions_after_decay = Hash_set.copy colony_locations in
      Hash_set.fold_until colony_locations ~init:size_decrease
        ~f:(fun decrease_left current_colony_position ->
          match decrease_left = 0 with
          | true -> Stop ()
          | false -> (
              Hash_set.remove positions_after_decay current_colony_position;
              match Hash_set.is_empty positions_after_decay with
              | true -> Stop ()
              | false -> (
                  let vistable =
                    dfs positions_after_decay
                      (get_random_position_from_hash_set positions_after_decay)
                  in
                  match
                    Hash_set.length vistable
                    = Hash_set.length positions_after_decay
                  with
                  | true -> Continue (decrease_left - 1)
                  | false ->
                      Hash_set.add positions_after_decay current_colony_position;
                      Continue decrease_left)))
        ~finish:(fun decrease_left -> print_s [%message (decrease_left : int)]);
      print_time_diff "decay" start_time;
      positions_after_decay
(* let shuffled_position_array = knuth_shuffle (Set.to_array colony_locations) in
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
  final_locations *)
