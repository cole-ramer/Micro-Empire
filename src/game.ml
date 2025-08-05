open! Core

module Enemy_target = struct
  module Target_type = struct
    type t = Enemy | Nutrient [@@deriving equal, sexp]
  end

  type t = {
    target_type : Target_type.t;
    closest_pos_in_source_colony : Position.t;
    closest_pos_in_target_colony : Position.t;
    distance : int;
  }
end

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : (int, Colony.t) Hashtbl.t;
  enemy_targets : (int, Enemy_target.t) Hashtbl.t;
  nutrients : Position.Hash_Set.t list;
  board : Board.t;
  creation_id_generator : Creation_id.t;
  time_of_last_move_of_enemies : (int, Time_ns.t) Hashtbl.t;
}

let get_empty_positions (game : t) =
  let all_positions_list =
    List.init game.board.width ~f:(fun x ->
        List.init game.board.height ~f:(fun y -> { Position.x; y }))
    |> List.concat
  in
  (* ask tas*)
  let all_enemies =
    Hashtbl.fold game.enemies ~init:(Position.Hash_Set.create ())
      ~f:(fun ~key ~data current_enemies ->
        Hash_set.union data.locations current_enemies)
  in
  let player_locations = game.player.locations in
  let all_nutrients_set = Position.Hash_Set.create () in
  let all_nutrients_set =
    List.fold game.nutrients ~init:all_nutrients_set
      ~f:(fun current_set new_set -> Hash_set.union current_set new_set)
  in
  let all_empty_positons = Position.Hash_Set.create () in
  let all_filled_positions = Position.Hash_Set.create () in
  List.iter all_positions_list ~f:(fun position ->
      match
        Hash_set.mem all_enemies position
        || Hash_set.mem player_locations position
        || Hash_set.mem all_nutrients_set position
      with
      | false -> Hash_set.add all_empty_positons position
      | true -> Hash_set.add all_filled_positions position);
  (all_empty_positons, all_filled_positions)

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
    let random_nutrient_size game =
      Float.to_int
        ((1. +. Random.float 5.)
        *. Float.log10 (Int.to_float game.player.size +. 1.))

    let new_nutrient_positions game =
      let empty_positions, filled_positions = get_empty_positions game in
      let starting_position =
        Util.get_random_position_from_hash_set empty_positions
      in

      let spawn_size = random_nutrient_size game in
      let inital_set = Position.Hash_Set.create () in
      Hash_set.add inital_set starting_position;

      Util.expand_randomly ~current_colony_locations:inital_set
        ~all_game_filled_positions:filled_positions game.board
        ~size_increase:spawn_size;
      inital_set

    let nutrient_replace (game : t) (nutrient_position_to_replace : Position.t)
        =
      let updated_nutrients : Position.Hash_Set.t list =
        List.map game.nutrients ~f:(fun nutrient_cluster ->
            match
              ( Hash_set.mem nutrient_cluster nutrient_position_to_replace,
                Hash_set.length nutrient_cluster = 1 )
            with
            | true, true -> new_nutrient_positions game
            | true, false ->
                Hash_set.remove nutrient_cluster nutrient_position_to_replace;
                nutrient_cluster
            | false, _ -> nutrient_cluster)
      in
      let nutrients_to_produce =
        (game.board.width / 3) - List.length game.nutrients
      in
      if nutrients_to_produce > 0 then
        let empty_list = List.init nutrients_to_produce ~f:Fn.id in
        List.fold empty_list ~init:{ game with nutrients = updated_nutrients }
          ~f:(fun new_game _ ->
            {
              new_game with
              nutrients =
                List.append new_game.nutrients
                  [ new_nutrient_positions new_game ];
            })
      else { game with nutrients = updated_nutrients }
  end

  module Enemy = struct
    let random_enemy_spawn_size player_size =
      match player_size with 0 -> 1 | _ -> Random.int player_size + 1

    let random_enemy_energy spawn_size = Random.int (1 * spawn_size)

    let initial_locations starting_position
        (filled_positions : Position.Hash_Set.t) spawn_size board =
      let inital_locations = Position.Hash_Set.create () in
      Hash_set.add inital_locations starting_position;
      Util.expand_randomly ~current_colony_locations:inital_locations
        ~all_game_filled_positions:filled_positions board
        ~size_increase:spawn_size;
      inital_locations

    let starting_level spawn_size = Random.int ((spawn_size / 10) + 1)

    let create_new_enemy game =
      let empty_positions, filled_positions = get_empty_positions game in
      let num_of_empty_positions = Hash_set.length empty_positions in
      let game =
        if num_of_empty_positions < game.player.size then upgrade_board game
        else game
      in
      let starting_position =
        Util.get_random_position_from_hash_set empty_positions
      in
      let initial_locations = Position.Hash_Set.create () in
      Hash_set.add initial_locations starting_position;
      let spawn_size = random_enemy_spawn_size game.player.size in
      Util.expand_randomly ~current_colony_locations:initial_locations
        ~all_game_filled_positions:filled_positions game.board
        ~size_increase:spawn_size;
      let new_enemy : Colony.t =
        {
          energy = random_enemy_energy spawn_size;
          size = spawn_size;
          locations = initial_locations;
          nutrient_absorption_level = starting_level spawn_size;
          decay_reduction_level = starting_level spawn_size;
          movement_level = starting_level spawn_size;
          strength_level = starting_level spawn_size;
          peak_size = spawn_size;
        }
      in
      let new_enemy_id = Creation_id.next_id game.creation_id_generator in
      let _ = Hashtbl.add game.enemies ~key:new_enemy_id ~data:new_enemy in
      Hashtbl.add_exn game.time_of_last_move_of_enemies ~key:new_enemy_id
        ~data:(Time_ns.now ());

      game

    let enemy_replace game ~(enemy_id_to_remove : int) =
      let game_with_new_enemy = create_new_enemy game in
      Hashtbl.remove game_with_new_enemy.enemies enemy_id_to_remove;
      Hashtbl.remove game_with_new_enemy.time_of_last_move_of_enemies
        enemy_id_to_remove;

      let enemies_to_produce =
        (game.board.width / 6) - Hashtbl.length game.enemies
      in
      if enemies_to_produce > 0 then
        let empty_list = List.init enemies_to_produce ~f:Fn.id in
        List.fold empty_list ~init:game_with_new_enemy ~f:(fun new_game _ ->
            create_new_enemy new_game)
      else game_with_new_enemy
  end
end

module Environment = struct
  let check_nutrient_consumptions game =
    let old_nutrient_locations =
      List.fold game.nutrients ~init:(Position.Hash_Set.create ())
        ~f:(fun current_set position_set ->
          Hash_set.union current_set position_set)
    in
    (* handles consumption for the player *)
    let new_game =
      Hash_set.fold game.player.locations ~init:game
        ~f:(fun current_game player_position ->
          match Hash_set.mem old_nutrient_locations player_position with
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
      Hashtbl.fold (Hashtbl.copy game.enemies) ~init:new_game
        ~f:(fun ~key ~data current_game ->
          Hash_set.fold data.locations ~init:current_game
            ~f:(fun current_game enemy_position ->
              let best_target_to_nutrient, game =
                Hash_set.fold old_nutrient_locations
                  ~init:
                    ( {
                        Enemy_target.closest_pos_in_source_colony =
                          { x = -1; y = -1 };
                        Enemy_target.closest_pos_in_target_colony =
                          { x = -1; y = -1 };
                        distance = Int.max_value;
                        target_type = Nutrient;
                      },
                      current_game )
                  ~f:(fun
                      (best_nutrient_target_so_far, inner_game)
                      current_nutrient_position
                    ->
                    let current_distance =
                      Position.get_distance enemy_position
                        current_nutrient_position
                    in
                    match
                      ( current_distance,
                        current_distance < best_nutrient_target_so_far.distance
                      )
                    with
                    | 0, _ ->
                        let energized_enemy = Colony.consume_nutrient data in
                        Hashtbl.set inner_game.enemies ~key
                          ~data:energized_enemy;
                        ( best_nutrient_target_so_far,
                          Spawning.Nutrient.nutrient_replace inner_game
                            enemy_position )
                    | _, true ->
                        ( {
                            Enemy_target.closest_pos_in_source_colony =
                              enemy_position;
                            closest_pos_in_target_colony =
                              current_nutrient_position;
                            distance = current_distance;
                            target_type = Nutrient;
                          },
                          inner_game )
                    | _, false -> (best_nutrient_target_so_far, inner_game))
              in
              match best_target_to_nutrient.distance = Int.max_value with
              | true ->
                  raise_s
                    [%message "distance to nutirent should not be infinite"]
              | false ->
                  Hashtbl.set game.enemy_targets ~key
                    ~data:best_target_to_nutrient;
                  game))
    in
    new_game

  let colonies_overlap_and_update_targets
      (enemy_target_map : (int, Enemy_target.t) Hashtbl.t) ~(colony1 : Colony.t)
      ~(colony1_id : int) ~(colony2 : Colony.t) =
    let best_target_between_colonies =
      Hash_set.fold_until colony1.locations
        ~init:
          {
            Enemy_target.closest_pos_in_source_colony = { x = -1; y = -1 };
            Enemy_target.closest_pos_in_target_colony = { x = -1; y = -1 };
            Enemy_target.distance = Int.max_value;
            target_type = Enemy;
          }
        ~f:(fun best_target_so_far position_in_colony1 ->
          let inner_best_target =
            Hash_set.fold_until colony2.locations ~init:best_target_so_far
              ~f:(fun inner_best_target_so_far position_in_colony2 ->
                let distance =
                  Position.get_distance position_in_colony1 position_in_colony2
                in
                match distance with
                | 0 ->
                    Stop
                      {
                        Enemy_target.closest_pos_in_source_colony =
                          position_in_colony1;
                        Enemy_target.closest_pos_in_target_colony =
                          position_in_colony2;
                        distance;
                        target_type = Enemy;
                      }
                | _ -> (
                    match distance < inner_best_target_so_far.distance with
                    | true ->
                        Continue
                          {
                            Enemy_target.closest_pos_in_source_colony =
                              position_in_colony1;
                            Enemy_target.closest_pos_in_target_colony =
                              position_in_colony2;
                            distance;
                            target_type = Enemy;
                          }
                    | false -> Continue inner_best_target_so_far))
              ~finish:(fun target -> target)
          in

          match inner_best_target.distance with
          | 0 -> Stop inner_best_target
          | _ -> Continue inner_best_target)
        ~finish:(fun best_target -> best_target)
    in
    let best_distance_colony1_colony2 = best_target_between_colonies.distance in
    if best_distance_colony1_colony2 = Int.max_value then false
    else
      match
        (best_distance_colony1_colony2, Hashtbl.find enemy_target_map colony1_id)
      with
      | 0, _ -> true
      | _, None ->
          Hashtbl.set enemy_target_map ~key:colony1_id
            ~data:best_target_between_colonies;
          false
      | _, Some current_best_total_target -> (
          match
            best_distance_colony1_colony2 < current_best_total_target.distance
            && colony1.size >= colony2.size
          with
          | true ->
              Hashtbl.set enemy_target_map ~key:colony1_id
                ~data:best_target_between_colonies;
              false
          | false -> false)

  let%expect_test "colonies overlap on shared position" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony1 =
      {
        colony1 with
        locations =
          Position.Hash_Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    let colony2 =
      {
        colony2 with
        locations =
          Position.Hash_Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    print_s
      [%message
        (colonies_overlap_and_update_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| true |}]

  let%expect_test "colonies do not overlap" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony1 =
      {
        colony1 with
        locations =
          Position.Hash_Set.of_list
            [ { Position.x = 0; y = 0 }; { Position.x = 0; y = 1 } ];
      }
    in
    let colony2 =
      {
        colony2 with
        locations =
          Position.Hash_Set.of_list
            [ { Position.x = 1; y = 0 }; { Position.x = 1; y = 1 } ];
      }
    in
    print_s
      [%message
        (colonies_overlap_and_update_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| false |}]

  let%expect_test "empty colony does not overlap" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    let colony2 =
      {
        colony2 with
        locations = Position.Hash_Set.of_list [ { Position.x = 1; y = 1 } ];
      }
    in
    print_s
      [%message
        (colonies_overlap_and_update_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| false |}]

  let%expect_test "both colonies empty" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    print_s
      [%message
        (colonies_overlap_and_update_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| false |}]

  let handle_fights_for_one_enemy_colony game ~enemy_id ~enemy_colony
      (current_enemy_map : (int, Colony.t) Hashtbl.t) :
      (int, Colony.t) Hashtbl.t =
    let enemy_targets = game.enemy_targets in
    let map_after_player_fights = Hashtbl.copy current_enemy_map in
    Hashtbl.iteri current_enemy_map ~f:(fun ~key ~data ->
        let enemy2_id = key in
        let enemy2 = data in
        match
          Hashtbl.mem map_after_player_fights enemy_id
          && colonies_overlap_and_update_targets
               (Hashtbl.create (module Int))
               ~colony1:enemy_colony ~colony1_id:enemy_id ~colony2:enemy2
          && not (enemy_id = enemy2_id)
        with
        | false -> ()
        | true -> (
            let outer_enemy_result, inner_enemy_result =
              Colony.fight ~colony1:enemy_colony ~colony2:enemy2
            in
            match (outer_enemy_result, inner_enemy_result) with
            | Some outer_enemy, None -> (
                Hashtbl.remove map_after_player_fights enemy2_id;
                Hashtbl.remove game.time_of_last_move_of_enemies enemy2_id;
                Hashtbl.set map_after_player_fights ~key:enemy_id
                  ~data:outer_enemy;
                match
                  ( Hashtbl.find enemy_targets enemy_id,
                    Hashtbl.find enemy_targets enemy2_id )
                with
                | None, None -> ()
                | Some target, None -> ()
                | None, Some target ->
                    Hashtbl.add_exn enemy_targets ~key:enemy_id ~data:target
                | Some target1, Some target2 -> (
                    match target1.distance > target2.distance with
                    | true ->
                        Hashtbl.set enemy_targets ~key:enemy_id ~data:target2;
                        Hashtbl.remove enemy_targets enemy2_id
                    | false -> Hashtbl.remove enemy_targets enemy2_id))
            | None, Some inner_enemy ->
                Hashtbl.remove map_after_player_fights enemy_id;
                Hashtbl.remove game.time_of_last_move_of_enemies enemy_id;
                Hashtbl.set map_after_player_fights ~key ~data:inner_enemy
            | _, _ ->
                raise_s
                  [%message
                    (outer_enemy_result : Colony.t option)
                      (inner_enemy_result : Colony.t option)
                      "is an invalid return from Colony.fight, there must be \
                       exactly one winner"]));
    map_after_player_fights

  let handle_fights game : t =
    let enemy_map = game.enemies in

    (* Player vs Enemy fights*)
    let enemy_colony_map_after_player_fights = Hashtbl.copy enemy_map in
    let player_after_fights =
      Hashtbl.fold enemy_map ~init:(Some game.player)
        ~f:(fun ~key ~data current_player ->
          match current_player with
          | None -> None
          | Some current_player -> (
              match
                colonies_overlap_and_update_targets game.enemy_targets
                  ~colony1:data ~colony1_id:key ~colony2:current_player
              with
              | false -> Some current_player
              | true -> (
                  let player_result, enemy_colony_result =
                    Colony.fight ~colony1:current_player ~colony2:data
                  in
                  match (player_result, enemy_colony_result) with
                  | Some player, None ->
                      Hashtbl.remove enemy_colony_map_after_player_fights key;
                      Hashtbl.remove game.time_of_last_move_of_enemies key;
                      Some player
                  | None, Some enemy -> None
                  | _, _ ->
                      raise_s
                        [%message
                          (player_result : Colony.t option)
                            (enemy_colony_result : Colony.t option)
                            "is an invalid return from Colony.fight, there \
                             must be exactly one winner"])))
    in
    (* let enemies_with_targets = Hashtbl.keys game.time_of_last_move_of_enemies in
    (match player_after_fights with
    | None -> ()
    | Some player -> print_s [%message (player.size : int)]);
    print_s [%message (enemies_with_targets : int list)]; *)
    (* All enemy vs. enemy fights*)
    let enemy_map_after_enemy_fights =
      Hashtbl.copy enemy_colony_map_after_player_fights
    in
    let enemy_map_after_enemy_fights =
      Hashtbl.fold enemy_colony_map_after_player_fights
        ~init:enemy_map_after_enemy_fights
        ~f:(fun ~key ~data current_enemy_map ->
          match Hashtbl.mem current_enemy_map key with
          | true ->
              handle_fights_for_one_enemy_colony game ~enemy_id:key
                ~enemy_colony:data current_enemy_map
          | false -> current_enemy_map)
    in

    match player_after_fights with
    | None ->
        {
          game with
          game_state =
            Game_state.Game_over ("You lost the fight", game.player.peak_size);
          enemies = enemy_map_after_enemy_fights;
          player =
            Colony.create_empty_colony ~peak_size:game.player.peak_size ();
        }
    | Some player ->
        let game_after_fights =
          { game with enemies = enemy_map_after_enemy_fights; player }
        in
        (* Replaces defeated colonies *)
        let num_of_enemies_to_replace =
          Hashtbl.length enemy_map - Hashtbl.length enemy_map_after_enemy_fights
        in
        let placeholder_list = List.init num_of_enemies_to_replace ~f:Fn.id in
        List.fold placeholder_list ~init:game_after_fights
          ~f:(fun current_game _ ->
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
      | false, false -> game)

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
    let table_copy = Hashtbl.copy game.enemies in
    Hashtbl.iteri table_copy ~f:(fun ~key ~data ->
        let enemy_id = key in
        let enemy_colony = data in
        let time_since_last_move =
          Time_ns.diff (Time_ns.now ())
            (Hashtbl.find_exn game.time_of_last_move_of_enemies key)
        in
        match
          Time_ns.Span.( >= ) time_since_last_move (Time_ns.Span.of_ms 1500.0)
        with
        | true ->
            let target = Hashtbl.find_exn game.enemy_targets enemy_id in
            print_s [%message (target.target_type : Enemy_target.Target_type.t)];
            let direction_towards_target =
              List.random_element_exn
                (Dir.possible_dir_to_reach_target
                   ~source:target.closest_pos_in_source_colony
                   ~target:target.closest_pos_in_target_colony)
            in
            let new_colony =
              Colony.move enemy_colony game.board direction_towards_target
            in
            Hashtbl.set game.enemies ~key ~data:new_colony;
            Hashtbl.set game.time_of_last_move_of_enemies ~key
              ~data:(Time_ns.now ())
        | false -> ())
end

let handle_key game char =
  let upgrade_player upgrade =
    match Colony.upgrade game.player upgrade with
    | Some upgraded_colony -> Some { game with player = upgraded_colony }
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
  | '1' -> upgrade_player Upgrades.Strength
  | '2' -> upgrade_player Upgrades.Movement
  | '3' -> upgrade_player Upgrades.Nutrient_absorption
  | '4' -> upgrade_player Upgrades.Decay_reduction
  | 'w' -> move_player Dir.Up
  | 'a' -> move_player Dir.Left
  | 's' -> move_player Dir.Down
  | 'd' -> move_player Dir.Right
  | _ -> Some game

let update_environment game =
  let game = { game with enemy_targets = Hashtbl.create (module Int) } in
  let nutrients_consumed = Environment.check_nutrient_consumptions game in
  let game_after_fights = Environment.handle_fights nutrients_consumed in

  Enemy_behaviour.move_all_enemies game_after_fights;

  let player_after_decay = Colony.decay game_after_fights.player in

  let game = evaluate { game_after_fights with player = player_after_decay } in

  game

(*hardcoded before implementation*)
let create ~width ~height =
  let creation_id_generator = Creation_id.create () in
  let locations = Position.Hash_Set.create () in
  Hash_set.add locations { x = 0; y = 0 };
  let game =
    {
      player =
        {
          size = 1;
          locations;
          energy = 500;
          nutrient_absorption_level = 1;
          decay_reduction_level = 1;
          strength_level = 1;
          movement_level = 1;
          peak_size = 1;
        };
      game_state = Game_state.In_progress;
      enemies = Hashtbl.create (module Int);
      enemy_targets = Hashtbl.create (module Int);
      nutrients = [ Position.Hash_Set.create () ];
      board = { width; height };
      creation_id_generator;
      time_of_last_move_of_enemies = Hashtbl.create (module Int);
    }
  in
  let new_nutrients =
    List.init 10 ~f:(fun _ -> Spawning.Nutrient.new_nutrient_positions game)
  in
  let game_with_nutrients = { game with nutrients = new_nutrients } in
  let list_of_three = List.init 6 ~f:Fn.id in

  List.fold list_of_three ~init:game_with_nutrients ~f:(fun updated_game _ ->
      Spawning.Enemy.create_new_enemy updated_game)
