(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

include DISK_NODE
open Types

val of_string : string -> [ area | project | action ]
val to_string : [< area | project | action ] -> string

val contact_of_string : string -> contact
val contact_to_string : contact -> string

val context_of_string : string -> context
val context_to_string : context -> string

val equal : ([< generic] as 'a) -> 'a -> bool

val make_action : state:action_state -> ?context:Ck_id.t -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> action]
val make_project : state:project_state -> ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> project]
val make_area : ?contact:Ck_id.t -> name:string -> description:string -> parent:Ck_id.t -> ctime:float -> unit -> [> area]
val make_contact : name:string -> description:string -> ctime:float -> unit -> contact
val make_context : name:string -> description:string -> ctime:float -> unit -> context

val with_name : generic -> string -> generic
val with_description : generic -> string -> generic
val with_parent : [< area | project | action] -> Ck_id.t -> [area | project | action]
val with_contact : [< area | project | action] -> Ck_id.t option -> [area | project | action]
val with_repeat : action -> Ck_time.repeat option -> [> action]
val with_astate : action -> action_state -> action
val with_pstate : project -> [ `Active | `SomedayMaybe | `Done ] -> project
val with_starred : [< project | action] -> bool -> [project | action]
val with_context : action -> Ck_id.t option -> action

val as_area : project -> area
val as_project : [< area | action] -> project
val as_action : project -> action

val merge : ?base:[< area | project | action] -> theirs:[< area | project | action] -> [< area | project | action] ->
  [area | project | action]
val merge_context : ?base:context -> theirs:context -> context -> context
val merge_contact : ?base:contact -> theirs:contact -> contact -> contact
val with_conflict : string -> ([< generic] as 'a) -> 'a
val without_conflicts : ([< generic] as 'a) -> 'a
