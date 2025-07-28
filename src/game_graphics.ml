open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
end

let only_one : bool ref = ref false
let insufficient_energy_error : int ref = ref 0

module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let sidebar_width = 200. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 675. *. scaling_factor |> Float.iround_down_exn
  let block_size = 27. *. scaling_factor |> Float.iround_down_exn
end

let draw_block { Position.x; y } ~color =
  let open Constants in
  let draw_area x_start y_start input_block_size ~input_color =
    Graphics.set_color input_color;
    let x1, y1, x2, y2 =
      (x_start + 1, y_start + 1, input_block_size - 1, input_block_size - 1)
    in
    Graphics.fill_rect x1 y1 x2 y2
  in
  draw_area (x * block_size) (y * block_size) block_size ~input_color:color;
  let decor_boxes =
    [
      (1, 1, 10000); (3, 5, 20000); (6, 6, 13000); (5, 3, -6000); (7, 1, 15000);
    ]
  in
  let decor_size = block_size / 9 in
  List.iter decor_boxes ~f:(fun (x_off, y_off, color_off) ->
      draw_area
        ((x * block_size) + (x_off * decor_size))
        ((y * block_size) + (y_off * decor_size))
        decor_size ~input_color:(color + color_off))

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one then failwith "Can only call init_exn once" else only_one := true;
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_width + sidebar_width) play_area_height);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Graphics.set_window_title "Micro Empire";
  Graphics.set_text_size (15. *. scaling_factor |> Float.iround_down_exn);
  Game.create ~height ~width

let render (game : Game.t) =
  Graphics.clear_graph ();
  let open Constants in
  Graphics.set_color 0;
  Graphics.fill_rect 0 0 play_area_width play_area_height;
  Set.iter game.player.locations ~f:(fun { x; y } ->
      draw_block { x; y } ~color:1352489);
  Map.iter game.enemies ~f:(fun enemy ->
      Set.iter enemy.locations ~f:(fun { x; y } ->
          draw_block { x; y } ~color:10687515));
  Set.iter game.nutrients ~f:(fun { x; y } ->
      draw_block { x; y } ~color:15248896);
  draw_block
    {
      x = (play_area_width / block_size) + 3;
      y = (play_area_height / block_size) - 2;
    }
    ~color:1352489;
  draw_block { x = (play_area_width / block_size) + 5; y = 4 } ~color:15248896;
  draw_block { x = (play_area_width / block_size) + 5; y = 2 } ~color:10687515;
  let player = game.player in
  let text =
    [
      "   YOU: ";
      "";
      "Energy: " ^ Int.to_string player.energy;
      "Size: " ^ Int.to_string player.size ^ " CELLS";
      "Strength: Level " ^ Int.to_string player.strength_level;
      "Nutrient Absorption: Level "
      ^ Int.to_string player.nutrient_absorption_level;
      "Decay Reduction: Level " ^ Int.to_string player.decay_reduction_level;
      "Movement: Level " ^ Int.to_string player.movement_level;
      "";
      "UPGRADE MUTATIONS";
      "(Press # to Upgrade)";
      "1. Strength : 10";
      "2. Size : 10";
      "4. Free Movement : 10";
      "5. Nutrient Absorption: 10";
      "6. Decay Reduction : 10";
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
  if !insufficient_energy_error > 0 then (
    Graphics.moveto (play_area_width / 2) (play_area_height - 50);
    Graphics.draw_string "NOT ENOUGH ENERGY")
  else ()

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let set_error duration = insufficient_energy_error := duration

let fade_error_message () =
  if !insufficient_energy_error > 0 then
    insufficient_energy_error := !insufficient_energy_error - 1
  else ()
