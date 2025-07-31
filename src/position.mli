open! Core

type t = { x : int; y : int } [@@deriving sexp, compare, equal, hash]

include Comparable.S with type t := t

module Table : sig
  include Hashtbl.S with type key := t
end

module Hash_Set : sig
  include Hash_set.S with type elt := t
end

val position_right : t -> t
val position_left : t -> t
val position_up : t -> t
val position_down : t -> t
val adjacent_positions : t -> Set.t
val get_distance : t -> t -> int
