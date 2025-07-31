open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
end

let only_one : bool ref = ref false
let block_size : int ref = ref 27
let insufficient_energy_error : int ref = ref 0

module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 648. *. scaling_factor |> Float.iround_down_exn
  let sidebar_width = 250. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 648. *. scaling_factor |> Float.iround_down_exn
  let virtual_map_width = 30
  let virtual_map_height = 30
end

let draw_block { Position.x; y } ~board_size ?(no_decor : bool option) ~color
    ~header (player : Colony.t) =
  let block_size =
    if header then 27 else if !block_size = 0 then 2 else !block_size
  in
  let open Constants in
  let window_block_width = play_area_width / block_size in
  let window_block_height = play_area_height / block_size in
  let x, y =
    if header then (x, y)
    else
      let { Position.x = center_x; y = center_y } = Colony.center player in
      let center_x =
        if center_x < window_block_width / 2 then window_block_width / 2
        else center_x
      in
      let center_y =
        if center_y < window_block_height / 2 then window_block_height / 2
        else center_y
      in

      let center_x =
        if center_x > board_size - (window_block_width / 2) then
          board_size - (window_block_width / 2)
        else center_x
      in
      let center_y =
        if center_y > board_size - (window_block_height / 2) then
          board_size - (window_block_height / 2)
        else center_y
      in
      let x = x - center_x + (window_block_width / 2) in
      let y = y - center_y + (window_block_height / 2) in
      (x, y)
  in
  if header || (x < window_block_width && y < window_block_height) then (
    let draw_area x_start y_start input_block_size ~input_color =
      Graphics.set_color input_color;
      let x1, y1, x2, y2 =
        (x_start + 1, y_start + 1, input_block_size - 1, input_block_size - 1)
      in
      Graphics.fill_rect x1 y1 x2 y2
    in
    draw_area (x * block_size) (y * block_size) block_size ~input_color:color;
    match no_decor with
    | None ->
        let decor_boxes =
          [
            (1, 1, 10000);
            (3, 5, 20000);
            (6, 6, 13000);
            (5, 3, -6000);
            (7, 1, 15000);
          ]
        in
        let decor_size = (block_size + 8) / 9 in
        List.iter decor_boxes ~f:(fun (x_off, y_off, color_off) ->
            draw_area
              ((x * block_size) + (x_off * decor_size))
              ((y * block_size) + (y_off * decor_size))
              decor_size ~input_color:(color + color_off))
    | _ -> ())

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one then failwith "Can only call init_exn once" else only_one := true;
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_width + sidebar_width) play_area_height);
  let height = virtual_map_height in
  let width = virtual_map_width in
  Graphics.set_window_title "Micro Empire";
  Graphics.set_text_size (15. *. scaling_factor |> Float.iround_down_exn);
  Graphics.auto_synchronize false;
  Game.create ~height ~width

let render (game : Game.t) =
  block_size := 648 * 3 / 2 / game.board.width;
  Graphics.clear_graph ();
  let open Constants in
  Graphics.set_color 0;
  Graphics.fill_rect 0 0 play_area_width play_area_height;

  Set.iter game.player.locations ~f:(fun { x; y } ->
      draw_block { x; y } ~color:1352489 ~board_size:game.board.width
        ~header:false game.player);
  Map.iter game.enemies ~f:(fun enemy ->
      Set.iter enemy.locations ~f:(fun { x; y } ->
          draw_block { x; y } ~color:10687515 ~board_size:game.board.width
            ~header:false game.player));
  List.iter game.nutrients ~f:(fun nutrient ->
      Set.iter nutrient ~f:(fun { x; y } ->
          draw_block { x; y } ~color:15248896 ~board_size:game.board.width
            ~header:false game.player));
  draw_block
    { x = (play_area_width / 27) + 3; y = (play_area_height / 27) - 2 }
    ~board_size:game.board.width ~header:true ~color:1352489 game.player;
  draw_block
    { x = (play_area_width / 27) + 5; y = 4 }
    ~board_size:game.board.width ~color:15248896 ~header:true game.player;
  draw_block
    { x = (play_area_width / 27) + 5; y = 2 }
    ~board_size:game.board.width ~color:10687515 ~header:true game.player;
  let player = game.player in
  let text =
    [
      "   YOU: ";
      "";
      "Energy: " ^ Int.to_string player.energy;
      "Size: " ^ Int.to_string player.size ^ " CELLS";
      "Strength: Level " ^ Int.to_string player.strength_level;
      "Movement Cost Reduction: Level " ^ Int.to_string player.movement_level;
      "Nutrient Absorption: Level "
      ^ Int.to_string player.nutrient_absorption_level;
      "Decay Reduction: Level " ^ Int.to_string player.decay_reduction_level;
      "";
      "UPGRADE MUTATIONS";
      "(Press # to Upgrade)";
      "1. Size : "
      ^ Int.to_string
          (Upgrades.upgrade_cost ~size:game.player.size Upgrades.Size);
      "2. Strength: "
      ^ Int.to_string
          (Upgrades.upgrade_cost ~level:game.player.strength_level
             Upgrades.Strength);
      "3. Movement Cost Reduction : "
      ^ Int.to_string
          (Upgrades.upgrade_cost ~level:game.player.movement_level
             Upgrades.Movement);
      "4. Nutrient Absorption: "
      ^ Int.to_string
          (Upgrades.upgrade_cost ~level:game.player.nutrient_absorption_level
             Upgrades.Nutrient_absorption);
      "5. Decay Reduction : "
      ^ Int.to_string
          (Upgrades.upgrade_cost ~level:game.player.decay_reduction_level
             Upgrades.Decay_reduction);
    ]
  in
  Graphics.set_color 0;
  Graphics.moveto (play_area_width + 20) (play_area_height - 50);
  List.iter text ~f:(fun str ->
      Graphics.draw_string str;
      Graphics.current_y () |> fun y ->
      Graphics.moveto (play_area_width + 20) (y - 25));
  let key = [ "  NUTRIENT : "; ""; "  ENEMY BACTERIA : " ] in
  Graphics.moveto (play_area_width + 20) 119;
  List.iter key ~f:(fun str ->
      Graphics.draw_string str;
      Graphics.current_y () |> fun y ->
      Graphics.moveto (play_area_width + 20) (y - 27));
  Graphics.set_color 10687515;
  Graphics.moveto (play_area_width / 2) (play_area_height - 50);
  (match game.game_state with
  | In_progress ->
      if !insufficient_energy_error > 0 then
        Graphics.draw_string "NOT ENOUGH ENERGY"
  | Game_over (reason, peak_size) ->
      Graphics.set_color 16777215;
      Graphics.moveto (play_area_width / 2) (play_area_height - 50);
      Graphics.draw_string reason;
      Graphics.moveto ((play_area_width / 2) + 20) (play_area_height - 75);
      Graphics.draw_string ("Peak Size: " ^ Int.to_string peak_size));

  Graphics.synchronize ()

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let set_error duration = insufficient_energy_error := duration

let fade_error_message () =
  if !insufficient_energy_error > 0 then
    insufficient_energy_error := !insufficient_energy_error - 1
  else ()

let expand_visual () = block_size := !block_size - 9
