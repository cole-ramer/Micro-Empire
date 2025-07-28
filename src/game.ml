open! Core

type t = {
  player : Colony.t;
  game_state : Game_state.t;
  enemies : Colony.t list;
  nutrients : Position.Set.t;
  board : Board.t;
}

let get_empty_positions (game : t) =
  let all_positions_set =
    List.init game.board.width ~f:(fun x ->
        List.init game.board.height ~f:(fun y -> { Position.x; y }))
    |> List.concat |> Position.Set.of_list
  in
  let all_enemies_set =
    List.map game.enemies ~f:(fun enemy -> enemy.locations)
    |> Position.Set.union_list
  in
  let all_occupied_positions =
    Set.union game.nutrients all_enemies_set |> Set.union game.player.locations
  in
  Set.diff all_positions_set all_occupied_positions

let nutrient_replace (game : t) (nutrient_position : Position.t) =
  let possible_positions = get_empty_positions game in
  let number_of_possible_positions = Set.length possible_positions in
  let random_index = Random.int number_of_possible_positions in
  let new_nutrient_positon_option = Set.nth possible_positions random_index in
  let without_old_nutrient = Set.remove game.nutrients nutrient_position in
  let new_nutrient_position_set =
    match new_nutrient_positon_option with
    | Some new_nutrient_position -> Set.add game.nutrients new_nutrient_position
    | None -> without_old_nutrient
  in
  { game with nutrients = new_nutrient_position_set }

module Enemy_spawning = struct
  let random_enemy_spawn_size = Random.int 6
  let random_enemy_energy spawn_size = Random.int (75 * spawn_size)

  let inital_locations set_of_starting_point spawn_size board =
    Util.expand_randomly set_of_starting_point board ~size_increase:spawn_size

  let starting_level spawn_size = Random.int ((spawn_size / 10) + 1)

  let get_starting_enemy game =
    let possible_starting_position = Set.choose (get_empty_positions game) in
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
        let new_enemy_list = new_enemy :: game.enemies in
        { game with enemies = new_enemy_list }
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
  | '5' -> upgrade_player Upgrades.Decary_reduction
  | 'w' -> move_player Dir.Up
  | 'a' -> move_player Dir.Left
  | 's' -> move_player Dir.Down
  | 'd' -> move_player Dir.Right
  | _ -> Some game

let update_environment game = game

(*hardcoded before implementation*)
let create ~width ~height =
  {
    player =
      {
        size = 3;
        locations =
          Position.Set.of_list
            [ { x = 0; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 1 } ];
        energy = 1000;
        nutrient_absorption_level = 1;
        decay_reduction_level = 1;
        strength_level = 1;
        movement_level = 1;
      };
    game_state = Game_state.In_progress;
    enemies =
      [
        {
          size = 3;
          locations =
            Position.Set.of_list
              [
                { x = 6; y = 7 };
                { x = 7; y = 7 };
                { x = 7; y = 6 };
                { x = 7; y = 8 };
              ];
          energy = 10000;
          nutrient_absorption_level = 1;
          decay_reduction_level = 1;
          strength_level = 1;
          movement_level = 1;
        };
      ];
    nutrients = Position.Set.of_list [ { x = 5; y = 5 } ];
    board = { width = 10; height = 10 };
  }
