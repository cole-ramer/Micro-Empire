open! Core

type t = Right | Left | Up | Down [@@deriving sexp]

let possible_dir_to_reach_target ~(source : Position.t) ~(target : Position.t) =
  print_s [%message (source : Position.t) (target : Position.t)];
  let list_of_dirs = [ Right; Left; Up; Down ] in
  let possible_dirs =
    List.filter list_of_dirs ~f:(fun dir ->
        match dir with
        | Right -> target.x > source.x
        | Left -> target.x < source.x
        | Up -> target.y > source.y
        | Down -> target.y < source.y)
  in
  match List.length possible_dirs > 2 with
  | true ->
      raise_s
        [%message
          "can somehow move in more than on direction for any give dimension \
           towards target"]
  | false -> possible_dirs
