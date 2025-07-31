open! Core

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : Colony.t Int.Map.t;
  (* change to a list of sets*)
  nutrients : Position.Set.t list;
  board : Board.t;
  creation_id_generator : Creation_id.t;
}

let get_empty_positions (game : t) =
  let all_positions_set =
    List.init game.board.width ~f:(fun x ->
        List.init game.board.height ~f:(fun y -> { Position.x; y }))
    |> List.concat |> Position.Set.of_list
  in
  (* ask tas*)
  let all_enemies_set =
    Map.fold game.enemies ~init:Position.Set.empty
      ~f:(fun ~key ~data current_enemy_positions ->
        ignore key;
        Set.union current_enemy_positions data.locations)
  in
  let all_nutrients_set = Position.Set.union_list game.nutrients in
  let all_occupied_positions =
    Set.union all_nutrients_set all_enemies_set
    |> Set.union game.player.locations
  in
  Set.diff all_positions_set all_occupied_positions

let upgrade_board (game : t) =
  let current_board = game.board.width in
  if Colony.length game.player > current_board / 2 then
    {
      game with
      board =
        {
          Board.height =
            Float.to_int (Int.to_float current_board *. Float.sqrt 2.);
          width = Float.to_int (Int.to_float current_board *. Float.sqrt 2.);
        };
    }
  else game

module Spawning = struct
  module Nutrient = struct
    let random_nutrient_size = Random.int 4

    let new_nutrient_positions game =
      let empty_positions = get_empty_positions game in
      let loc_index = Random.int (Set.length empty_positions) in
      let possible_starting_position = Set.nth empty_positions loc_index in

      match possible_starting_position with
      | None -> Position.Set.empty
      | Some starting_position ->
          let spawn_size = random_nutrient_size in
          let inital_set = Set.add Position.Set.empty starting_position in
          let new_nutrients =
            Util.expand_randomly inital_set game.board ~size_increase:spawn_size
          in
          new_nutrients

    let nutrient_replace (game : t) (nutrient_position_to_replace : Position.t)
        =
      let updated_nutrients : Position.Set.t list =
        List.map game.nutrients ~f:(fun nutrient_cluster ->
            match
              ( Set.mem nutrient_cluster nutrient_position_to_replace,
                Set.length nutrient_cluster = 1 )
            with
            | true, true -> new_nutrient_positions game
            | true, false ->
                Set.remove nutrient_cluster nutrient_position_to_replace
            | false, _ -> nutrient_cluster)
      in
      { game with nutrients = updated_nutrients }
  end

  module Enemy = struct
    let random_enemy_spawn_size player_size =
      match player_size with 0 -> 1 | _ -> Random.int player_size + 1

    let random_enemy_energy spawn_size = Random.int (75 * spawn_size)

    let initial_locations set_of_starting_point spawn_size board =
      Util.expand_randomly set_of_starting_point board ~size_increase:spawn_size

    let starting_level spawn_size = Random.int ((spawn_size / 10) + 1)

    let create_new_enemy game =
      let num_pos = Set.length (get_empty_positions game) in
      let game =
        if num_pos < game.player.size then upgrade_board game else game
      in
      let loc_index = Random.int (Set.length (get_empty_positions game)) in
      let possible_starting_position =
        Set.nth (get_empty_positions game) loc_index
      in
      match possible_starting_position with
      | None -> game
      | Some starting_position ->
          let spawn_size = random_enemy_spawn_size game.player.size in
          let inital_set = Set.add Position.Set.empty starting_position in
          let new_enemy : Colony.t =
            {
              energy = random_enemy_energy spawn_size;
              size = spawn_size;
              locations = initial_locations inital_set spawn_size game.board;
              nutrient_absorption_level = starting_level spawn_size;
              decay_reduction_level = starting_level spawn_size;
              movement_level = starting_level spawn_size;
              strength_level = starting_level spawn_size;
              peak_size = spawn_size;
            }
          in
          let new_enemy_map =
            Map.add_exn game.enemies
              ~key:(Creation_id.next_id game.creation_id_generator)
              ~data:new_enemy
          in
          { game with enemies = new_enemy_map }

    let enemey_replace game ~(enemy_id_to_remove : int) =
      let game_with_new_enemy = create_new_enemy game in
      {
        game with
        enemies = Map.remove game_with_new_enemy.enemies enemy_id_to_remove;
      }
  end
end

module Environment = struct
  let check_nutrient_consumptions game =
    let old_nutrient_locations = Position.Set.union_list game.nutrients in
    (* handles consumption for the player *)
    let new_game =
      Set.fold game.player.locations ~init:game
        ~f:(fun current_game player_position ->
          match Set.mem old_nutrient_locations player_position with
          | true ->
              let game_with_nutrient_consumption =
                {
                  current_game with
                  player = Colony.consume_nutrient current_game.player;
                }
              in
              Spawning.Nutrient.nutrient_replace game_with_nutrient_consumption
                player_position
          | false -> current_game)
    in

    (* handles consumption for all the enemies*)
    let new_game =
      Map.fold game.enemies ~init:new_game ~f:(fun ~key ~data current_game ->
          Set.fold data.locations ~init:current_game
            ~f:(fun current_game enemy_position ->
              match Set.mem old_nutrient_locations enemy_position with
              | true ->
                  let energized_enemy = Colony.consume_nutrient data in
                  let new_enemy_map =
                    Map.set current_game.enemies ~key ~data:energized_enemy
                  in
                  let game_with_nutrient_consumption =
                    { current_game with enemies = new_enemy_map }
                  in
                  Spawning.Nutrient.nutrient_replace
                    game_with_nutrient_consumption enemy_position
              | false -> current_game))
    in
    new_game

  let two_colonies_overlap ~(colony1 : Colony.t) ~(colony2 : Colony.t) =
    Set.fold_until colony1.locations ~init:false
      ~f:(fun do_overlap position_in_colony1 ->
        match Set.mem colony2.locations position_in_colony1 with
        | true -> Stop true
        | false -> Continue false)
      ~finish:(fun bool -> bool)

  let%expect_test "colonies overlap on shared position" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony1 =
      {
        colony1 with
        locations =
          Position.Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    let colony2 =
      {
        colony2 with
        locations =
          Position.Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    print_s [%message (two_colonies_overlap ~colony1 ~colony2 : bool)];
    [%expect {| true |}]

  let%expect_test "colonies do not overlap" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony1 =
      {
        colony1 with
        locations =
          Position.Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    let colony2 =
      {
        colony2 with
        locations =
          Position.Set.of_list
            [ { Position.x = 1; y = 0 }; { Position.x = 1; y = 1 } ];
      }
    in
    print_s [%message (two_colonies_overlap ~colony1 ~colony2 : bool)];
    [%expect {| false |}]

  let%expect_test "empty colony does not overlap" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony2 =
      {
        colony2 with
        locations = Position.Set.of_list [ { Position.x = 1; y = 1 } ];
      }
    in
    print_s [%message (two_colonies_overlap ~colony1 ~colony2 : bool)];
    [%expect {| false |}]

  let%expect_test "both colonies empty" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    print_s [%message (two_colonies_overlap ~colony1 ~colony2 : bool)];
    [%expect {| false |}]

  let handle_fights_for_one_enemy_colony ~enemy_id ~enemy_colony
      (current_enemy_map : Colony.t Int.Map.t) : Colony.t Int.Map.t =
    Map.fold_until current_enemy_map ~init:current_enemy_map
      ~f:(fun ~key ~data inner_fold_enemy_map ->
        match
          ( Map.mem inner_fold_enemy_map key,
            two_colonies_overlap ~colony1:enemy_colony ~colony2:data,
            enemy_id = key )
        with
        | _, false, _ | false, _, _ | _, _, true ->
            Continue inner_fold_enemy_map
        | true, true, false -> (
            let outer_enemy_result, inner_enemy_result =
              (* print_s
                [%message "fight" (enemy_colony : Colony.t) (data : Colony.t)]; *)
              Colony.fight ~colony1:enemy_colony ~colony2:data
            in
            match (outer_enemy_result, inner_enemy_result) with
            | Some outer_enemy, None ->
                let without_loser_map = Map.remove inner_fold_enemy_map key in
                Continue
                  (Map.set without_loser_map ~key:enemy_id ~data:outer_enemy)
            | None, Some inner_enemy ->
                let without_loser_map =
                  Map.remove inner_fold_enemy_map enemy_id
                in
                Stop (Map.set without_loser_map ~key ~data:inner_enemy)
            | _, _ ->
                raise_s
                  [%message
                    (outer_enemy_result : Colony.t option)
                      (inner_enemy_result : Colony.t option)
                      "is an invalid return from Colony.fight, there must be \
                       exactly one winner"]))
      ~finish:(fun final_inner_enemy_map -> final_inner_enemy_map)

  let handle_fights game =
    let enemy_map = game.enemies in

    (* Player vs Enemy fights*)
    let game_after_player_fights =
      Map.fold enemy_map ~init:game ~f:(fun ~key ~data current_game ->
          match
            two_colonies_overlap ~colony1:current_game.player ~colony2:data
          with
          | false -> current_game
          | true -> (
              let player_result, enemy_colony_result =
                Colony.fight ~colony1:current_game.player ~colony2:data
              in
              match (player_result, enemy_colony_result) with
              | Some player, None ->
                  let updated_enemy_map = Map.remove current_game.enemies key in
                  { current_game with player; enemies = updated_enemy_map }
              | None, Some enemy ->
                  {
                    current_game with
                    player =
                      Colony.create_empty_colony
                        ~peak_size:game.player.peak_size ();
                    enemies = Map.set current_game.enemies ~key ~data:enemy;
                    game_state =
                      Game_state.Game_over
                        ("GAME OVER: You lost the fight", game.player.peak_size);
                  }
              | _, _ ->
                  raise_s
                    [%message
                      (player_result : Colony.t option)
                        (enemy_colony_result : Colony.t option)
                        "is an invalid return from Colony.fight, there must be \
                         exactly one winner"]))
    in
    (* All enemy vs. enemy fights*)
    let new_enemy_map =
      Map.fold game_after_player_fights.enemies
        ~init:game_after_player_fights.enemies
        ~f:(fun ~key ~data current_enemy_map ->
          match Map.mem current_enemy_map key with
          | true ->
              handle_fights_for_one_enemy_colony ~enemy_id:key
                ~enemy_colony:data current_enemy_map
          | false -> current_enemy_map)
    in
    let game_after_fights =
      { game_after_player_fights with enemies = new_enemy_map }
    in
    (* Replaces defeated colonies *)
    let num_of_enemies_to_replace =
      Map.length enemy_map - Map.length new_enemy_map
    in
    let placeholder_list = List.init num_of_enemies_to_replace ~f:Fn.id in
    List.fold placeholder_list ~init:game_after_fights ~f:(fun current_game _ ->
        Spawning.Enemy.create_new_enemy current_game)
end

let evaluate game =
  match game.game_state with
  | Game_state.Game_over _ -> game
  | Game_state.In_progress -> (
      match (game.player.energy <= 0, game.player.size <= 0) with
      | true, true ->
          let game_state =
            Game_state.Game_over
              ("GAME OVER: No energy and cells left", game.player.peak_size)
          in
          { game with game_state }
      | false, true ->
          let game_state =
            Game_state.Game_over
              ("GAME OVER: No cells left", game.player.peak_size)
          in
          { game with game_state }
      | true, false ->
          let game_state =
            Game_state.Game_over
              ("GAME OVER: No energy left", game.player.peak_size)
          in
          { game with game_state }
      | false, false -> (
          let number_of_nutrients =
            List.fold game.nutrients ~init:0 ~f:(fun count cluster ->
                count + Set.length cluster)
          in
          let number_of_enemies = Map.length game.enemies in
          let number_of_open_positions =
            Set.length (get_empty_positions game)
          in
          match
            number_of_enemies = 0 && number_of_nutrients = 0
            && number_of_open_positions = 0
          with
          | true ->
              let game_state =
                Game_state.Game_over
                  ("WINNER you occupy every cell!", game.player.peak_size)
              in
              { game with game_state }
          | false -> game))

let upgrade_board (game : t) =
  let current_board = game.board.width in
  if Colony.length game.player > current_board / 2 then
    {
      game with
      board =
        {
          Board.height =
            Float.to_int (Int.to_float current_board *. Float.sqrt 2.);
          width = Float.to_int (Int.to_float current_board *. Float.sqrt 2.);
        };
    }
  else game

module Enemy_behaviour = struct
  let move_all_enemies game =
    let random = Random.int 9 in
    match random = 0 with
    | true ->
        let moved_enemies =
          Map.map game.enemies ~f:(fun enemy ->
              let direction = List.random_element_exn Dir.directions_list in
              Colony.move enemy game.board direction)
        in
        { game with enemies = moved_enemies }
    | false -> game
end

let handle_key game char =
  let upgrade_player upgrade =
    match Colony.upgrade game.player upgrade with
    | Some upgraded_colony ->
        Some
          ({ game with player = upgraded_colony }
          |> Environment.check_nutrient_consumptions
          |> Environment.handle_fights |> evaluate)
    | None -> None
  in
  let move_player direction =
    let moved_colony = Colony.move game.player game.board direction in
    let movement_cost =
      Upgrades.upgrade_effect ~size:moved_colony.size
        ~level:moved_colony.movement_level Upgrades.Movement
    in
    match movement_cost >= moved_colony.energy with
    | false ->
        let moved_colony =
          { moved_colony with energy = moved_colony.energy - movement_cost }
        in

        Some
          ({ game with player = moved_colony }
          |> Environment.check_nutrient_consumptions
          |> Environment.handle_fights |> evaluate)
    | true ->
        let moved_colony = { moved_colony with energy = 0 } in
        Some
          {
            game with
            game_state =
              Game_over ("GAME OVER: No energy left", game.player.peak_size);
            player = moved_colony;
          }
  in

  match char with
  | '1' -> (
      match Colony.upgrade ~board:game.board game.player Upgrades.Size with
      | Some upgraded_size ->
          Some (evaluate { game with player = upgraded_size } |> upgrade_board)
      | None -> None)
  | '2' -> upgrade_player Upgrades.Strength
  | '3' -> upgrade_player Upgrades.Movement
  | '4' -> upgrade_player Upgrades.Nutrient_absorption
  | '5' -> upgrade_player Upgrades.Decay_reduction
  | 'w' -> move_player Dir.Up
  | 'a' -> move_player Dir.Left
  | 's' -> move_player Dir.Down
  | 'd' -> move_player Dir.Right
  | _ ->
      Some
        (game |> Environment.check_nutrient_consumptions
       |> Environment.handle_fights |> upgrade_board |> evaluate)

let update_environment game =
  let start_time = Time_ns.now () in
  let nutrients_consumed = Environment.check_nutrient_consumptions game in
  Util.print_time_diff "nutrients_consumed" start_time;
  let start_time = Time_ns.now () in
  let game_after_fights = Environment.handle_fights nutrients_consumed in
  Util.print_time_diff "fights_handled" start_time;
  (* let start_time = Time_ns.now () in *)
  (* let game_after_moves = Enemy_behaviour.move_all_enemies game_after_fights in
  Util.print_time_diff "enemy moves" start_time; *)
  let start_time = Time_ns.now () in
  let player_after_decay = Colony.decay game_after_fights.player in
  Util.print_time_diff "player decay" start_time;
  let game = evaluate { game_after_fights with player = player_after_decay } in

  game

(*hardcoded before implementation*)
let create ~width ~height =
  let creation_id_generator = Creation_id.create () in
  let game =
    {
      player =
        {
          size = 1;
          locations = Position.Set.of_list [ { x = 0; y = 0 } ];
          energy = 500;
          nutrient_absorption_level = 1;
          decay_reduction_level = 1;
          strength_level = 1;
          movement_level = 1;
          peak_size = 1;
        };
      game_state = Game_state.In_progress;
      enemies = Int.Map.empty;
      nutrients = [ Position.Set.empty ];
      board = { width; height };
      creation_id_generator;
    }
  in
  let new_nutrients =
    List.init 10 ~f:(fun _ -> Spawning.Nutrient.new_nutrient_positions game)
  in
  let game_with_nutrients = { game with nutrients = new_nutrients } in
  let list_of_three = List.init 10 ~f:Fn.id in

  List.fold list_of_three ~init:game_with_nutrients ~f:(fun updated_game _ ->
      Spawning.Enemy.create_new_enemy updated_game)
(* BEWARE: THIS IS A MONSTROSITY, ONLY THE MOST FEARLESS OF DEVELOPERS
SHALL DARE ATTEMPT TO SALVAGE THIS BUG RIDDLED SHIT MESS*)
(*module Enemy_behaviour = struct
  let get_avg_position (colony : Colony.t) =
    let total_x, total_y =
      Set.fold colony.locations ~init:(0, 0) ~f:(fun (x, y) position ->
          (x + position.x, y + position.y))
    in
    { Position.x = total_x / colony.size; y = total_y / colony.size }

  let get_avg_position_enemy_map game =
    Map.map game.enemies ~f:(fun enemy -> get_avg_position enemy)

  let nearest_nutrient nutrients (avg_colony_position : Position.t) =
    Set.fold (Position.Set.union_list nutrients) ~init:(Int.max_value, None)
      ~f:(fun (best_distance, best_position_option) nutrient ->
        let distance = Position.get_distance avg_colony_position nutrient in
        match distance < best_distance && distance > 0 with
        | true -> (distance, Some nutrient)
        | false -> (best_distance, best_position_option))

  let nearest_smaller_colony enemy_map ~(player : Colony.t) average_position_map
      ~enemy_id =
    let moving_enemy = Map.find_exn enemy_map enemy_id in
    let moving_enemy_avg_position = get_avg_position moving_enemy in
    let moving_enemy_size = moving_enemy.size in
    let nearest_enemy_distance, nearest_smaller_enemy_position =
      Map.fold enemy_map ~init:(Int.max_value, None)
        ~f:(fun ~key ~data (best_distance, best_enemy_option) ->
          if key = enemy_id then (
            print_endline "in da check";
            (best_distance, best_enemy_option))
          else
            let current_distance =
              Position.get_distance moving_enemy_avg_position
                (Map.find_exn average_position_map key)
            in
            match
              ( current_distance < best_distance && current_distance > 0,
                moving_enemy_size > (Map.find_exn enemy_map key).size )
            with
            | true, true ->
                (current_distance, Some (Map.find_exn average_position_map key))
            | _, _ -> (best_distance, best_enemy_option))
    in
    let avg_player_position = get_avg_position player in
    let distance_to_player =
      Position.get_distance moving_enemy_avg_position avg_player_position
    in
    match (nearest_smaller_enemy_position, player.size < moving_enemy_size) with
    | Some small_enemy_position, true -> (
        match nearest_enemy_distance < distance_to_player with
        | true -> (nearest_enemy_distance, Some small_enemy_position)
        | false -> (distance_to_player, Some avg_player_position))
    | None, false -> (nearest_enemy_distance, None)
    | Some small_enemy_position, false ->
        (nearest_enemy_distance, Some small_enemy_position)
    | None, true -> (distance_to_player, Some avg_player_position)

  let move_and_upgrade_colony ~game ~enemy_id ~(colony : Colony.t)
      ~(source : Position.t) ~(target : Position.t) ~(distance : int) =
    match
      List.random_element (Dir.possible_dir_to_reach_target ~source ~target)
    with
    | Some direction -> (
        let padded_cost_to_target =
          distance
          * Upgrades.upgrade_effect ~level:colony.movement_level
              ~size:colony.size Upgrades.Movement
          * 2
        in
        let hypothetical_colony =
          { colony with energy = colony.energy - padded_cost_to_target }
        in
        match Colony.move colony game.board direction with
        | None -> (game.enemies, [ enemy_id ])
        | Some moved_colony -> (
            match
              List.random_element (Colony.possible_upgrades hypothetical_colony)
            with
            | None -> (Map.set game.enemies ~key:enemy_id ~data:moved_colony, [])
            | Some upgrade ->
                let upgraded_colony =
                  (match upgrade with
                  | Upgrades.Size ->
                      Colony.upgrade ~board:game.board moved_colony upgrade
                  | _ -> Colony.upgrade moved_colony upgrade)
                  |> Option.value_exn
                in

                (Map.set game.enemies ~key:enemy_id ~data:upgraded_colony, [])))
    | None ->
        print_s
          [%message
            (Dir.possible_dir_to_reach_target ~source ~target : Dir.t list)];
        raise_s
          [%message
            "closest enemey did not have any direction to move towards to meet \
             it"]

  let move_enemy game enemy_id avg_position_map =
    let moving_colony = Map.find_exn game.enemies enemy_id in
    let enemy_avg_position = Map.find_exn avg_position_map enemy_id in
    let closest_nutrient_distance, closest_nutrient =
      nearest_nutrient game.nutrients enemy_avg_position
    in
    let closest_nutrient =
      match closest_nutrient with
      | Some nutrient -> nutrient
      | None ->
          raise_s
            [%message
              "There must be at least one nutrient the colony could move \
               towards"]
    in
    let closest_colony_distance, closest_colony_position =
      nearest_smaller_colony game.enemies ~player:game.player avg_position_map
        ~enemy_id
    in
    match
      ( closest_nutrient_distance > closest_colony_distance,
        closest_colony_position )
    with
    | true, Some colony_position ->
        print_endline "colony";
        move_and_upgrade_colony ~game ~enemy_id ~colony:moving_colony
          ~source:enemy_avg_position ~target:colony_position
          ~distance:closest_colony_distance
    | false, Some _ | false, None ->
        print_endline "Nutrient";
        move_and_upgrade_colony ~game ~enemy_id ~colony:moving_colony
          ~source:enemy_avg_position ~target:closest_nutrient
          ~distance:closest_nutrient_distance
    | true, None ->
        raise_s
          [%message
            "there cannot be a distance to colony that is less than the \
             distance to nutrient to a colony that is None"]

  let move_all_enemies game =
    let avg_position_map = get_avg_position_enemy_map game in
    print_s [%message (avg_position_map : Position.t Core.Int.Map.t)];
    let moved_and_upgraded_enemies_map, enemies_to_replace =
      Map.fold game.enemies ~init:(game.enemies, [])
        ~f:(fun ~key ~data (moved_enemies, enemies_to_replace) ->
          let moved_enemies, new_enemies_toreplace =
            move_enemy game key avg_position_map
          in
          (moved_enemies, enemies_to_replace @ new_enemies_toreplace))
    in
    List.fold enemies_to_replace
      ~init:{ game with enemies = moved_and_upgraded_enemies_map }
      ~f:(fun current_game enemy_id_to_remove ->
        Spawning.Enemy.enemey_replace current_game ~enemy_id_to_remove)
end *)
