(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type project_details
type action_details

type action = [`Action of action_details]
type project = [`Project of project_details]
type area = [`Area]

module Make(I : Irmin.BASIC with type key = string list and type value = string) : sig
  type t
  type 'a full_node

  val make : (string -> I.t) -> t Lwt.t

  val root : t -> [area] full_node

  val all_areas_and_projects : t -> [> area | project] full_node list

  val name : _ full_node -> string
  val full_name : [< area | project | action] full_node -> string
  val uuid : _ full_node -> string

  val actions : [< area | project] full_node -> [action] full_node list
  val projects : [< area | project] full_node -> [project] full_node list
  val areas : [area] full_node -> [area] full_node list

  val add_action : t -> parent:string -> name:string -> description:string -> unit Lwt.t
  val add_project : t -> parent:string -> name:string -> description:string -> unit Lwt.t
  val add_area : t -> parent:string -> name:string -> description:string -> unit Lwt.t
end
