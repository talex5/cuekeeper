(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Ck_sigs

type 'a t = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float with default(0.0);
  details : 'a;
} with sexp

let ctime t = t.ctime
let name t = t.name
let description t = t.description
let parent t = t.parent
let details t = t.details

type general_node =
  [ `Action of action_details
  | `Project of Ck_sigs.project_details
  | `Area ] t
  with sexp

let root = {
  parent = Ck_id.root;
  name = "All";
  description = "Root area";
  details = `Area;
  ctime = 0.0;
}

let of_string s = general_node_of_sexp (Sexplib.Sexp.of_string s)
let to_string (t : [< action | project | area] t) = Sexplib.Sexp.to_string (sexp_of_general_node (t :> general_node))

let make ~name ~description ~parent ~ctime ~details = {
  name;
  description;
  parent;
  ctime;
  details;
}

let with_name node name = {node with name}
let with_details node details = {node with details}
let equal : _ t -> _ t -> bool = (=)
