open! Core

(* This is the core logic that actually runs the game. We have implemented all of this for
   you, but feel free to read this file as a reference. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds) >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())

let handle_keys (game : Game.t) ~game_over =
  every ~stop:game_over 0.001 ~f:(fun () ->
      match Game_graphics.read_key () with
      | None -> ()
      | Some key ->
          let new_game = Game.handle_key game key in
          Game_graphics.render new_game)

let update_environment (game : Game.t) ~game_over =
  (* The argument of 0.1 passed to [every] means that every 0.1 seconds, we will call
     [Game.step] and re-render the game. Changing this timespan will allow us to change
     the speed of the game. *)
  every ~stop:game_over 0.1 ~f:(fun () ->
      let new_game = Game.update_environment game in
      Game_graphics.render new_game;
      match (new_game.game_state : Game_state.t) with
      | Game_over -> game_over := true
      | In_progress -> ())

let run () =
  (* let game = Game_graphics.init_exn () in
  Game_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over;
  update_environment game ~game_over *)
  ()
