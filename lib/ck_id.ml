(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std

type t = string [@@deriving sexp]

let mint =
  let seed = Random.State.make_self_init () in
  let gen = Uuidm.v4_gen seed in
  fun () -> Uuidm.to_string (gen ())

let to_string t = t
let of_string t = t
let fmt () t = t
let compare = String.compare

module M = Map.Make(String)
module S = Set.Make(String)
