open! Core

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : Colony.t Int.Map.t;
  nutrients : Position.Set.t;
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
  let all_occupied_positions =
    Set.union game.nutrients all_enemies_set |> Set.union game.player.locations
  in
  Set.diff all_positions_set all_occupied_positions

module Spawning = struct
  module Nutrient = struct
    let random_nutrient_size = Random.int 4

    let new_nutrient_positions game =
      let possible_starting_position = Set.choose (get_empty_positions game) in
      match possible_starting_position with
      | None -> game
      | Some starting_position ->
          let spawn_size = random_nutrient_size in
          let inital_set = Set.add Position.Set.empty starting_position in
          let new_nutrients =
            Util.expand_randomly inital_set game.board ~size_increase:spawn_size
          in
          { game with nutrients = Set.union new_nutrients game.nutrients }

    let nutrient_replace (game : t) (nutrient_position_to_replace : Position.t)
        =
      let game_with_new_nutrients = new_nutrient_positions game in
      {
        game with
        nutrients =
          Set.remove game_with_new_nutrients.nutrients
            nutrient_position_to_replace;
      }
  end

  module Enemy = struct
    let random_enemy_spawn_size = Random.int 5 + 1
    let random_enemy_energy spawn_size = Random.int (75 * spawn_size)

    let inital_locations set_of_starting_point spawn_size board =
      Util.expand_randomly set_of_starting_point board ~size_increase:spawn_size

    let starting_level spawn_size = Random.int ((spawn_size / 10) + 1)

    let create_new_enemy game =
      let loc_index = Random.int (Set.length (get_empty_positions game)) in
      let possible_starting_position =
        Set.nth (get_empty_positions game) loc_index
      in
      match possible_starting_position with
      | None -> game
      | Some starting_position ->
          let spawn_size = random_enemy_spawn_size in
          let inital_set = Set.add Position.Set.empty starting_position in
          let new_enemy : Colony.t =
            {
              energy = random_enemy_energy spawn_size;
              size = spawn_size;
              locations = inital_locations inital_set spawn_size game.board;
              nutrient_absorption_level = starting_level spawn_size;
              decay_reduction_level = starting_level spawn_size;
              movement_level = starting_level spawn_size;
              strength_level = starting_level spawn_size;
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
    let old_nutrient_locations = game.nutrients in
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
      ~finish:(fun bool -> false)

  let handle_fights_for_one_enemy_colony ~enemy_id ~enemy_colony
      (current_enemy_map : Colony.t Int.Map.t) : Colony.t Int.Map.t =
    Map.fold_until current_enemy_map ~init:current_enemy_map
      ~f:(fun ~key ~data inner_fold_enemy_map ->
        match
          ( Map.mem inner_fold_enemy_map key,
            two_colonies_overlap ~colony1:enemy_colony ~colony2:data )
        with
        | _, false | false, _ -> Continue inner_fold_enemy_map
        | true, true -> (
            let outer_enemy_result, inner_enemy_result =
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
                    player = Colony.create_empty_colony;
                    enemies = Map.set current_game.enemies ~key ~data:enemy;
                    game_state = Game_state.Game_over;
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

let handle_key game char =
  let upgrade_player upgrade =
    match Colony.upgrade game.player upgrade with
    | Some upgraded_colony -> Some { game with player = upgraded_colony }
    | None -> None
  in
  let move_player direction =
    match Colony.move game.player game.board direction with
    | Some moved_colony -> Some { game with player = moved_colony }
    | None -> None
  in
  match char with
  | '1' -> upgrade_player Upgrades.Strength
  | '2' -> (
      match Colony.upgrade ~board:game.board game.player Upgrades.Size with
      | Some upgraded_size -> Some { game with player = upgraded_size }
      | None -> None)
  | '3' -> upgrade_player Upgrades.Movement
  | '4' -> upgrade_player Upgrades.Nutrient_absorption
  | '5' -> upgrade_player Upgrades.Decay_reduction
  | 'w' -> move_player Dir.Up
  | 'a' -> move_player Dir.Left
  | 's' -> move_player Dir.Down
  | 'd' -> move_player Dir.Right
  | _ -> Some game

let update_environment game = Environment.check_nutrient_consumptions game

(*hardcoded before implementation*)
let create ~width ~height =
  let creation_id_generator = Creation_id.create () in
  let game =
    {
      player =
        {
          size = 1;
          locations = Position.Set.of_list [ { x = 0; y = 0 } ];
          energy = 1000;
          nutrient_absorption_level = 1;
          decay_reduction_level = 1;
          strength_level = 1;
          movement_level = 1;
        };
      game_state = Game_state.In_progress;
      enemies = Int.Map.empty;
      nutrients = Position.Set.of_list [ { x = 5; y = 5 } ];
      board = { width = 25; height = 22 };
      creation_id_generator;
    }
  in
  let list_of_three = List.init 3 ~f:Fn.id in
  let game_with_enemies =
    List.fold list_of_three ~init:game ~f:(fun updated_game _ ->
        Spawning.Enemy.create_new_enemy updated_game)
  in
  Spawning.Nutrient.new_nutrient_positions game_with_enemies
