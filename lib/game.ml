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
;;

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
;;

let handle_key game char = match char with 'W' | 'A' | 'S' | 'D' | _ -> game
let update_environment game = game
