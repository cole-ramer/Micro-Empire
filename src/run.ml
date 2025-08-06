open! Core

let difficulty = ref Difficulty.Easy

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

let rec run () =
  Game_graphics.close ();
  Game_graphics.main_menu !difficulty;
  let game_start = ref false in
  every ~stop:game_start 0.001 ~f:(fun () ->
      match Game_graphics.read_key () with
      | None -> ()
      | Some key -> (
          match key with
          | ' ' ->
              difficulty := Difficulty.next !difficulty;
              Game_graphics.main_menu !difficulty
          | 'b' ->
              game_start := true;
              start_game ()
          | _ -> ()))

and start_game () =
  let game : Game.t ref = ref (Game_graphics.init_exn !difficulty) in
  Game_graphics.render !game;
  let game_over = ref false in
  handle_keys game ~game_over;
  update_environment game ~game_over

and update_environment (game : Game.t ref) ~game_over =
  (* Called every 0.1 seconds to step the game and render *)
  every ~stop:game_over 0.1 ~f:(fun () ->
      Game_graphics.fade_error_message ();
      let new_game = Game.update_environment !game in
      Game_graphics.render new_game;
      game := new_game;
      match (new_game.game_state : Game_state.t) with
      | Game_over _ ->
          let open Async in
          game_over := true;
          Async.don't_wait_for
            (let%bind () = Clock.after (Time_float.Span.of_sec 1.5) in
             let rec wait_for_key () =
               match Game_graphics.read_key () with
               | None ->
                   let%bind () =
                     Async.Clock.after (Time_float.Span.of_sec 0.001)
                   in
                   wait_for_key ()
               | Some _key -> return (run ())
             in
             wait_for_key ())
      | In_progress -> ())

and handle_keys (game : Game.t ref) ~game_over =
  (* Called every 0.001 seconds to check for key presses *)
  every ~stop:game_over 0.01 ~f:(fun () ->
      match Game_graphics.read_key () with
      | None -> ()
      | Some key -> (
          match Game.handle_key !game key with
          | Some new_game ->
              let new_game = Game.upgrade_board new_game in
              Game_graphics.render new_game;
              game := new_game
          | None -> Game_graphics.set_error 15))
