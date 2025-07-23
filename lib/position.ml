open! Core
module T = struct 
  type t = {
  x : int
  ; y : int
} [@@deriving sexp, compare, equal]
end
include T

include Comparable.Make(T)