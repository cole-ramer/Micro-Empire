open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let red = Graphics.rgb 255 000 000
end

let only_one : bool ref = ref false

module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let sidebar_width = 75. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 675. *. scaling_factor |> Float.iround_down_exn
  let block_size = 27. *. scaling_factor |> Float.iround_down_exn
end

let draw_block { Position.x; y } ~color =
  let open Constants in
  let x = x * block_size in
  let y = y * block_size in
  Graphics.set_color color;
  let x1, y1, x2, y2 = (x + 1, y + 1, block_size - 1, block_size - 1) in
  Graphics.fill_rect x1 y1 x2 y2

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
  Set.iter game.nutrients ~f:(fun { x; y } -> draw_block { x; y } ~color:161616)

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
