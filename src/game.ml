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
