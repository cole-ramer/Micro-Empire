open! Core
type t = {
  x : int
  ; y : int
} [@@deriving sexp, compare, equal]
include Comparable.S with type t := t