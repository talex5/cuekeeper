(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

include DISK_NODE
open Types

val of_string : string -> [ area | project | action ]
val to_string : [< area | project | action ] -> string

val contact_of_string : string -> contact_node
val contact_to_string : contact_node -> string

val equal : ([< generic] as 'a) -> 'a -> bool

val make_action : state:Ck_id.t action_state -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> [> action]
val make_project : name:string -> description:string -> parent:Ck_id.t -> ctime:float -> [> project]
val make_area : name:string -> description:string -> parent:Ck_id.t -> ctime:float -> [> area]
val make_contact : name:string -> description:string -> ctime:float -> contact_node

val with_name : generic -> string -> generic
val with_description : generic -> string -> generic
val with_parent : generic -> Ck_id.t -> generic
val with_astate : action_node -> Ck_id.t action_state -> action_node
val with_pstate : project_node -> [ `Active | `SomedayMaybe | `Done ] -> project_node
val with_starred : [< project | action] -> bool -> [project | action]

val as_area : project_node -> area_node
val as_project : [< area | action] -> project_node
val as_action : project_node -> action_node
