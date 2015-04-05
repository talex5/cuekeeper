(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std

type t = string with sexp

let root = ""
let mint () = Uuidm.(create `V4 |> to_string)
let to_string t = t
let of_string t = t
let fmt () t = t
let compare = String.compare

module M = Map.Make(String)
module S = Set.Make(String)
