open! Core

val get_random_position_from_hash_set : Position.Hash_Set.t -> Position.t

(* Will do a more a intresting grow, expanding by size_increase to adjacent positions not only
to the intial set, but to the intermediate sets as the Position.Set.t expands*)

(* Will shrink the given Position.Set.t by removing a total of size_decrease elements.
The elements removed are chosen at randomf from positions in the given set that when 
removed do not cause the set to be come disjoint. If there are not size_decrease 
number of positiosn that if those parameters it will simply remove as many as it 
can *)
(* Will decrease the position_hash_set by size_decrease in a way that doesn't
make it adisjoint*)
val shrink_randomly :
  Position.Hash_Set.t -> size_decrease:int -> Position.Hash_Set.t

(*prints the string given along with the diff between
when print_time_diff is called and the Time_ns.t provided*)
val print_time_diff : string -> Time_ns.t -> unit
