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

let shrink_randomly (colony_locations : Position.Hash_Set.t) ~size_decrease =
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

      positions_after_decay
