open! Core
open! Game_lib

let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
