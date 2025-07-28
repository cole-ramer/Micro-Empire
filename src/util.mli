open! Core

(* Will simply grow by size_to_increase to positions adjacent to the given Position.Set.t *)
val increase_size :
  Position.Set.t -> Board.t -> size_increase:int -> Position.Set.t

(* Will do a more a intresting grow, expanding by size_increase to adjacent positions not only
to the intial set, but to the intermediate sets as the Position.Set.t expands*)
val expand_randomly :
  Position.Set.t -> Board.t -> size_increase:int -> Position.Set.t
