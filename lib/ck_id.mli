(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type t = private string with sexp

val root : t
val mint : unit -> t
val to_string : t -> string
val of_string : string -> t
val fmt : unit -> t -> string
val compare : t -> t -> int

module M : Map.S with type key = t
module S : Set.S with type elt = t
