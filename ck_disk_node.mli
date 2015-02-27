(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

type +'a t = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float with default(0.0);
  details : 'a;
}
(** A generic node, with a field for type-specific details. *)

include DISK_NODE
  with type 'a t := 'a t

val of_string : string -> [action | project | area] t
val to_string : [< action | project | area] t -> string

val root : [> area] t
(** It's convenient to give every node a parent of type [t].
 * This can be used for the root to avoid needing options everywhere. *)

val make : name:string -> description:string -> parent:Ck_id.t -> ctime:float -> details:'a -> 'a t

val equal : 'a t -> 'a t -> bool
