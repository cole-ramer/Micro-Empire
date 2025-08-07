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
  board : Board.t;
  creation_id_generator : Creation_id.t;
  nutrient_position_id_map : (Position.t, int) Hashtbl.t;
  nutrient_id_cluster_map : (int, Position.Hash_Set.t) Hashtbl.t;
  time_of_last_move_of_enemies : (int, Time_ns.t) Hashtbl.t;
  difficulty : Difficulty.t;
}

(* Tracks function runtimes for optimization help*)
let function_duration_tracker : (string, Time_ns.Span.t * int) Hashtbl.t =
  Hashtbl.create (module String)

(* Global variables that are constantly updated for runtime improvment *)
let empty_positions : Position.Hash_Set.t = Position.Hash_Set.create ()
let filled_positions : Position.Hash_Set.t = Position.Hash_Set.create ()

let add_time_to_duration_tracker ~function_name ~start_time =
  let time_diff = Time_ns.diff (Time_ns.now ()) start_time in
  let current_time, current_calls =
    Hashtbl.find_or_add function_duration_tracker function_name
      ~default:(fun () -> (Time_ns.Span.of_int_ns 0, 0))
  in
  Hashtbl.set function_duration_tracker ~key:function_name
    ~data:(Time_ns.Span.( + ) time_diff current_time, current_calls + 1)

let print_function_duration_tracker () =
  Hashtbl.iteri function_duration_tracker ~f:(fun ~key ~data ->
      let total_time, number_of_calls = data in
      let function_name = key in
      let avg_time =
        Time_ns.Span.to_ns total_time /. Float.of_int number_of_calls
        |> Time_ns.Span.of_ns |> Time_ns.Span.to_short_string
      in
      let total_time = Time_ns.Span.to_short_string total_time in
      print_s
        [%message
          "function name:" function_name "avg time: " avg_time "total"
            total_time])

let get_empty_positions_at_start_of_game (game : t) =
  let start_time = Time_ns.now () in
  let all_positions_list =
    List.init game.board.width ~f:(fun x ->
        List.init game.board.height ~f:(fun y -> { Position.x; y }))
    |> List.concat
  in
  let all_enemies =
    Hashtbl.fold game.enemies ~init:(Position.Hash_Set.create ())
      ~f:(fun ~key ~data current_enemies ->
        Hash_set.union data.locations current_enemies)
  in
  let player_locations = game.player.locations in
  let all_nutrients = Position.Hash_Set.create () in
  let all_nutrients =
    Hashtbl.fold game.nutrient_id_cluster_map ~init:all_nutrients
      ~f:(fun ~key ~data current_set ->
        ignore key;
        Hash_set.union current_set data)
  in

  List.iter all_positions_list ~f:(fun position ->
      match
        Hash_set.mem all_enemies position
        || Hash_set.mem player_locations position
        || Hash_set.mem all_nutrients position
      with
      | false -> Hash_set.add empty_positions position
      | true -> Hash_set.add filled_positions position);
  add_time_to_duration_tracker ~function_name:"get_empty_positions" ~start_time

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

(* Updates global variables based on new and old locations of
a colony*)
let adjust_empty_and_filled_positions ~old_locations ~new_locations =
  let start_time = Time_ns.now () in
  Hash_set.iter old_locations ~f:(fun old_location ->
      match Hash_set.mem new_locations old_location with
      | true -> ()
      | false ->
          Hash_set.add empty_positions old_location;
          Hash_set.remove filled_positions old_location);
  Hash_set.iter new_locations ~f:(fun new_location ->
      match Hash_set.mem old_locations new_location with
      | true -> ()
      | false ->
          Hash_set.add filled_positions new_location;
          Hash_set.remove empty_positions new_location);
  add_time_to_duration_tracker
    ~function_name:"adjust_empty_and_filled_positions" ~start_time

module Spawning = struct
  (* Note: updates global empty_positions, and filled_positions
within the function *)
  let expand_colony_or_nutrient_cluster
      ~(current_colony_locations : Position.Hash_Set.t) (board : Board.t)
      ~size_increase =
    let new_positions = current_colony_locations in
    let available_positions = Position.Hash_Set.create () in
    Hash_set.iter current_colony_locations ~f:(fun colony_position ->
        Set.iter (Position.adjacent_positions colony_position)
          ~f:(fun adjacent_position ->
            match
              (not (Hash_set.mem filled_positions adjacent_position))
              && Board.is_in_bounds board adjacent_position
            with
            | true -> Hash_set.add available_positions adjacent_position
            | false -> ()));
    if Hash_set.length available_positions = 0 then ()
    else
      List.init size_increase ~f:Fn.id
      |> List.iter ~f:(fun _ ->
             if Hash_set.length available_positions = 0 then ()
             else
               let pos_to_add =
                 Util.get_random_position_from_hash_set available_positions
               in
               Hash_set.add new_positions pos_to_add;
               Hash_set.add filled_positions pos_to_add;
               Hash_set.remove empty_positions pos_to_add;
               Set.iter (Position.adjacent_positions pos_to_add)
                 ~f:(fun adjacent_position ->
                   match
                     (not (Hash_set.mem filled_positions adjacent_position))
                     && Board.is_in_bounds board adjacent_position
                   with
                   | true -> Hash_set.add available_positions adjacent_position
                   | false -> ()))

  module Nutrient = struct
    let random_nutrient_size game =
      Float.to_int
        ((1. +. Random.float 5.)
        *. Float.log10 (Int.to_float game.player.size +. 1.))

    let new_nutrient_positions game =
      let start_time_main = Time_ns.now () in
      print_endline "in new_nutrient_positions";
      let starting_position =
        Util.get_random_position_from_hash_set empty_positions
      in

      let spawn_size = random_nutrient_size game in
      let inital_set = Position.Hash_Set.create () in
      Hash_set.add inital_set starting_position;
      let start_time = Time_ns.now () in

      expand_colony_or_nutrient_cluster ~current_colony_locations:inital_set
        game.board ~size_increase:spawn_size;
      let new_position_id = Creation_id.next_id game.creation_id_generator in
      Hashtbl.add_exn game.nutrient_id_cluster_map ~key:new_position_id
        ~data:inital_set;
      Hash_set.iter inital_set ~f:(fun position ->
          match
            Hashtbl.add game.nutrient_position_id_map ~key:position
              ~data:new_position_id
          with
          | `Ok -> ()
          | `Duplicate ->
              raise_s
                [%message
                  "There should not be a duplicate position in the hash_map"]);

      add_time_to_duration_tracker ~function_name:"expand_randomly" ~start_time;
      add_time_to_duration_tracker ~function_name:"new_nutrient_positions"
        ~start_time:start_time_main

    let nutrient_replace (game : t) (nutrient_position_to_replace : Position.t)
        =
      let start_time_replace = Time_ns.now () in
      let nutrient_position_id =
        Hashtbl.find_exn game.nutrient_position_id_map
          nutrient_position_to_replace
      in
      let nutrient_cluster =
        Hashtbl.find_exn game.nutrient_id_cluster_map nutrient_position_id
      in
      Hash_set.remove nutrient_cluster nutrient_position_to_replace;
      Hashtbl.remove game.nutrient_position_id_map nutrient_position_to_replace;
      Hash_set.remove filled_positions nutrient_position_to_replace;
      Hash_set.add empty_positions nutrient_position_to_replace;
      match Hash_set.is_empty nutrient_cluster with
      | true ->
          Hashtbl.remove game.nutrient_id_cluster_map nutrient_position_id;

          new_nutrient_positions game
      | false ->
          ();

          (* Need to increase total # of nutrients*)
          let nutrients_to_produce =
            let expected_nutrients =
              match game.difficulty with
              | Easy -> game.board.width * 2 / 3
              | Medium -> game.board.width / 2
              | Hard -> game.board.width / 3
            in
            expected_nutrients - Hashtbl.length game.nutrient_position_id_map
          in
          if nutrients_to_produce > 0 then
            let _ =
              List.init nutrients_to_produce ~f:(fun num ->
                  new_nutrient_positions game)
            in

            add_time_to_duration_tracker ~function_name:"nutrient_replace"
              ~start_time:start_time_replace
          else
            add_time_to_duration_tracker ~function_name:"nutrient_replace"
              ~start_time:start_time_replace
  end

  module Enemy = struct
    let random_enemy_spawn_size player_size =
      match player_size with 0 -> 1 | _ -> Random.int player_size + 1

    let random_enemy_energy spawn_size = Random.int (1 * spawn_size)

    let initial_locations starting_position
        (filled_positions : Position.Hash_Set.t) spawn_size board =
      let start_time_inital_locations = Time_ns.now () in
      let inital_locations = Position.Hash_Set.create () in
      Hash_set.add inital_locations starting_position;
      let start_time_expand = Time_ns.now () in
      expand_colony_or_nutrient_cluster
        ~current_colony_locations:inital_locations board
        ~size_increase:spawn_size;
      add_time_to_duration_tracker ~function_name:"expand_randomly"
        ~start_time:start_time_expand;
      add_time_to_duration_tracker ~function_name:"initial_locations"
        ~start_time:start_time_inital_locations

    let starting_level spawn_size = Random.int ((spawn_size / 10) + 1)
    (* Note: updates empty and filled positions*)

    let create_new_enemy game =
      let start_time_new_enemy = Time_ns.now () in

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
      let start_time_expand = Time_ns.now () in
      expand_colony_or_nutrient_cluster
        ~current_colony_locations:initial_locations game.board
        ~size_increase:spawn_size;
      add_time_to_duration_tracker ~function_name:"expand_randomly"
        ~start_time:start_time_expand;
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
      add_time_to_duration_tracker ~function_name:"create_new_enemy"
        ~start_time:start_time_new_enemy;
      game
  end
end

module Environment = struct
  let check_nutrient_consumptions game =
    let start_time = Time_ns.now () in

    (* handles consumption for the player *)
    let new_player =
      Hash_set.fold game.player.locations ~init:game.player
        ~f:(fun current_player player_position ->
          match Hashtbl.mem game.nutrient_position_id_map player_position with
          | true ->
              Spawning.Nutrient.nutrient_replace game player_position;
              Colony.consume_nutrient current_player
          | false -> current_player)
    in
    let game = { game with player = new_player } in

    (* handles consumption for all the enemies*)
    let old_nutrient_positions = Hashtbl.copy game.nutrient_position_id_map in

    Hashtbl.iteri (Hashtbl.copy game.enemies) ~f:(fun ~key ~data ->
        Hash_set.iter data.locations ~f:(fun enemy_position ->
            Hashtbl.iter_keys old_nutrient_positions
              ~f:(fun current_nutrient_position ->
                let current_distance =
                  Position.get_distance enemy_position current_nutrient_position
                in

                match current_distance with
                | 0 ->
                    let energized_enemy = Colony.consume_nutrient data in
                    Hashtbl.set game.enemies ~key ~data:energized_enemy;
                    (* If distance is 0, it will be consumed, and therefore
                          will not be a possible target next round*)
                    Spawning.Nutrient.nutrient_replace game enemy_position
                | _ -> (
                    match Hashtbl.find game.enemy_targets key with
                    | None ->
                        Hashtbl.set game.enemy_targets ~key
                          ~data:
                            {
                              Enemy_target.closest_pos_in_source_colony =
                                enemy_position;
                              closest_pos_in_target_colony =
                                current_nutrient_position;
                              distance = current_distance;
                              target_type = Nutrient;
                            }
                    | Some current_best_target ->
                        if current_distance < current_best_target.distance then
                          Hashtbl.set game.enemy_targets ~key
                            ~data:
                              {
                                Enemy_target.closest_pos_in_source_colony =
                                  enemy_position;
                                closest_pos_in_target_colony =
                                  current_nutrient_position;
                                distance = current_distance;
                                target_type = Nutrient;
                              }
                        else ()))));

    add_time_to_duration_tracker ~function_name:"check_nutrient_consumptions"
      ~start_time;
    game

  let do_colonies_overlap_and_update_colony1_targets
      (enemy_target_map : (int, Enemy_target.t) Hashtbl.t) ~(colony1 : Colony.t)
      ~(colony1_id : int) ~(colony2 : Colony.t) =
    let start_time = Time_ns.now () in
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

    if best_distance_colony1_colony2 = Int.max_value then (
      add_time_to_duration_tracker
        ~function_name:"colonies_overlap_and_update_targets" ~start_time;
      false)
    else
      match
        (best_distance_colony1_colony2, Hashtbl.find enemy_target_map colony1_id)
      with
      | 0, _ ->
          add_time_to_duration_tracker
            ~function_name:"colonies_overlap_and_update_targets" ~start_time;
          true
      | _, None ->
          Hashtbl.set enemy_target_map ~key:colony1_id
            ~data:best_target_between_colonies;
          add_time_to_duration_tracker
            ~function_name:"colonies_overlap_and_update_targets" ~start_time;
          false
      | _, Some current_best_total_target -> (
          match
            best_distance_colony1_colony2 < current_best_total_target.distance
            && colony1.size >= colony2.size
          with
          | true ->
              Hashtbl.set enemy_target_map ~key:colony1_id
                ~data:best_target_between_colonies;
              add_time_to_duration_tracker
                ~function_name:"colonies_overlap_and_update_targets" ~start_time;
              false
          | false ->
              add_time_to_duration_tracker
                ~function_name:"colonies_overlap_and_update_targets" ~start_time;
              false)

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
        (do_colonies_overlap_and_update_colony1_targets
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
        (do_colonies_overlap_and_update_colony1_targets
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
        (do_colonies_overlap_and_update_colony1_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| false |}]

  let%expect_test "both colonies empty" =
    let colony1 = Colony.create_empty_colony () in
    let colony2 = Colony.create_empty_colony () in
    print_s
      [%message
        (do_colonies_overlap_and_update_colony1_targets
           (Hashtbl.create (module Int))
           ~colony1 ~colony1_id:1 ~colony2
          : bool)];
    [%expect {| false |}]

  let handle_fights_for_one_enemy_colony game ~enemy_id
      ~enemy_colony:enemy_colony1 (current_enemies : (int, Colony.t) Hashtbl.t)
      : (int, Colony.t) Hashtbl.t =
    let time_diff_required = Time_ns.Span.of_ms 110. in
    let start_time = Time_ns.now () in
    let enemy_targets = game.enemy_targets in
    let enemies_after_player_fights = Hashtbl.copy current_enemies in
    Hashtbl.iteri current_enemies ~f:(fun ~key ~data ->
        let enemy2_id = key in
        let enemy_colony2 = data in
        let now = Time_ns.now () in
        match
          (Hashtbl.mem enemies_after_player_fights enemy_id
          && not (enemy_id = enemy2_id))
          && (Time_ns.Span.( <= )
                (Time_ns.diff
                   (Hashtbl.find_exn game.time_of_last_move_of_enemies enemy_id)
                   now)
                time_diff_required
             || Time_ns.Span.( <= )
                  (Time_ns.diff
                     (Hashtbl.find_exn game.time_of_last_move_of_enemies
                        enemy2_id)
                     now)
                  time_diff_required)
        with
        | true -> (
            match
              do_colonies_overlap_and_update_colony1_targets enemy_targets
                ~colony1:enemy_colony1 ~colony1_id:enemy_id
                ~colony2:enemy_colony2
            with
            | false -> ()
            | true -> (
                let outer_enemy_result, inner_enemy_result =
                  Colony.fight ~colony1:enemy_colony1 ~colony2:enemy_colony2
                in
                match (outer_enemy_result, inner_enemy_result) with
                | Some outer_enemy, None -> (
                    Hashtbl.remove enemies_after_player_fights enemy2_id;
                    Hashtbl.remove game.time_of_last_move_of_enemies enemy2_id;

                    Hashtbl.set enemies_after_player_fights ~key:enemy_id
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
                            Hashtbl.set enemy_targets ~key:enemy_id
                              ~data:target2;
                            Hashtbl.remove enemy_targets enemy2_id
                        | false -> Hashtbl.remove enemy_targets enemy2_id))
                | None, Some inner_enemy ->
                    Hashtbl.remove enemies_after_player_fights enemy_id;
                    Hashtbl.remove game.time_of_last_move_of_enemies enemy_id;
                    Hashtbl.set enemies_after_player_fights ~key
                      ~data:inner_enemy
                | _, _ ->
                    raise_s
                      [%message
                        (outer_enemy_result : Colony.t option)
                          (inner_enemy_result : Colony.t option)
                          "is an invalid return from Colony.fight, there must \
                           be exactly one winner"]))
        | false -> ());
    add_time_to_duration_tracker
      ~function_name:"handle_fights_for_one_enemy_colony" ~start_time;
    enemies_after_player_fights

  let handle_fights game : t =
    let start_time = Time_ns.now () in
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
                do_colonies_overlap_and_update_colony1_targets
                  game.enemy_targets ~colony1:data ~colony1_id:key
                  ~colony2:current_player
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
        print_function_duration_tracker ();
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
        let game =
          List.fold placeholder_list ~init:game_after_fights
            ~f:(fun current_game _ ->
              Spawning.Enemy.create_new_enemy current_game)
        in
        add_time_to_duration_tracker ~function_name:"handle_fights" ~start_time;
        game
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
          print_function_duration_tracker ();
          { game with game_state }
      | false, true ->
          let game_state =
            Game_state.Game_over
              ("GAME OVER: No cells left", game.player.peak_size)
          in
          print_function_duration_tracker ();
          { game with game_state }
      | true, false ->
          let game_state =
            Game_state.Game_over
              ("GAME OVER: No energy left", game.player.peak_size)
          in
          print_function_duration_tracker ();
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
    let start_time = Time_ns.now () in
    let table_copy = Hashtbl.copy game.enemies in
    Hashtbl.iteri table_copy ~f:(fun ~key ~data ->
        let enemy_id = key in
        let enemy_colony = data in
        let time_since_last_move =
          Time_ns.diff (Time_ns.now ())
            (Hashtbl.find_exn game.time_of_last_move_of_enemies key)
        in
        let speed =
          match game.difficulty with
          | Easy -> 1500.
          | Medium -> 1100.
          | Hard -> 700.
        in
        match
          Time_ns.Span.( >= ) time_since_last_move (Time_ns.Span.of_ms speed)
        with
        | true ->
            let target = Hashtbl.find_exn game.enemy_targets enemy_id in

            let direction_towards_target =
              List.random_element_exn
                (Dir.possible_dirs_to_reach_target
                   ~source:target.closest_pos_in_source_colony
                   ~target:target.closest_pos_in_target_colony)
            in
            let new_colony =
              Colony.move enemy_colony game.board direction_towards_target
            in
            adjust_empty_and_filled_positions
              ~old_locations:enemy_colony.locations
              ~new_locations:new_colony.locations;
            Hashtbl.set game.enemies ~key ~data:new_colony;
            Hashtbl.set game.time_of_last_move_of_enemies ~key
              ~data:(Time_ns.now ())
        | false -> ());
    add_time_to_duration_tracker ~function_name:"move_all_enemies" ~start_time
end

let handle_key game char =
  let start_time = Time_ns.now () in
  let upgrade_player upgrade =
    match Colony.upgrade game.player upgrade with
    | Some upgraded_colony -> Some { game with player = upgraded_colony }
    | None -> None
  in
  let move_player direction =
    let moved_colony = Colony.move game.player game.board direction in
    let old_player = game.player in
    let movement_cost =
      let skew =
        match game.difficulty with Easy -> 0 | Medium -> 2 | Hard -> 5
      in
      skew
      + Upgrades.upgrade_effect ~size:moved_colony.size
          ~level:moved_colony.movement_level Upgrades.Movement
    in
    match movement_cost >= moved_colony.energy with
    | false ->
        let moved_colony =
          { moved_colony with energy = moved_colony.energy - movement_cost }
        in

        adjust_empty_and_filled_positions ~old_locations:old_player.locations
          ~new_locations:moved_colony.locations;
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
              (match game.game_state with
              | Game_over _ -> game.game_state
              | _ ->
                  Game_over ("GAME OVER: No energy left", game.player.peak_size));
            player = moved_colony;
          }
  in

  let game =
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
  in
  add_time_to_duration_tracker ~function_name:"handle_key" ~start_time;
  game

let update_environment game =
  let start_time = Time_ns.now () in
  let game = { game with enemy_targets = Hashtbl.create (module Int) } in
  let nutrients_consumed = Environment.check_nutrient_consumptions game in
  let game_after_fights = Environment.handle_fights nutrients_consumed in

  Enemy_behaviour.move_all_enemies game_after_fights;
  let decay_start = Time_ns.now () in
  let player_after_decay = Colony.decay game_after_fights.player in
  add_time_to_duration_tracker ~function_name:"decay" ~start_time:decay_start;
  adjust_empty_and_filled_positions
    ~old_locations:game_after_fights.player.locations
    ~new_locations:player_after_decay.locations;

  let game = evaluate { game_after_fights with player = player_after_decay } in
  add_time_to_duration_tracker ~function_name:"update_enviroment" ~start_time;
  game

(*hardcoded before implementation*)
let create ~width ~height ~difficulty =
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
      nutrient_position_id_map = Hashtbl.create (module Position);
      nutrient_id_cluster_map = Hashtbl.create (module Int);
      board = { width; height };
      creation_id_generator;
      time_of_last_move_of_enemies = Hashtbl.create (module Int);
      difficulty;
    }
  in
  get_empty_positions_at_start_of_game game;
  let num_of_nutrients =
    match game.difficulty with
    | Easy -> game.board.width * 2 / 3
    | Medium -> game.board.width / 2
    | Hard -> game.board.width / 3
  in
  let _ =
    List.init num_of_nutrients ~f:(fun _ ->
        Spawning.Nutrient.new_nutrient_positions game)
  in

  let num_of_enemies =
    match game.difficulty with Easy -> 4 | Medium -> 5 | Hard -> 6
  in
  let empty_list = List.init num_of_enemies ~f:Fn.id in

  let game =
    List.fold empty_list ~init:game ~f:(fun updated_game _ ->
        Spawning.Enemy.create_new_enemy updated_game)
  in

  game
