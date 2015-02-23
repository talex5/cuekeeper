(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type uuid = string

type action_details = {
  astate : [ `Next | `Waiting | `Future | `Done ]
} with sexp

type project_details = {
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp


module type MODEL = sig
  type t
  type 'a full_node

  type action = [`Action of action_details]
  type project = [`Project of project_details]
  type area = [`Area]

  type node_view = {
    uuid : uuid;
    node_type : [ area | project | action | `Deleted ] React.S.t;
    ctime : float;
    name : string React.S.t;
    child_views : node_view ReactiveData.RList.t;
  }

  type details = {
    details_uuid : uuid;
    details_type : [ area | project | action | `Deleted ] React.S.t;
    details_name : string React.S.t;
    details_description : string React.S.t;
    details_children : node_view ReactiveData.RList.t;
  }

  val root : t -> [area] full_node React.S.t
  val is_root : uuid -> bool

  val all_areas_and_projects : t -> (string * [> area | project] full_node) list

  val name : _ full_node -> string
  val uuid : _ full_node -> string

  val actions : [< area | project] full_node -> [action] full_node list
  val projects : [< area | project] full_node -> [project] full_node list
  val areas : [area] full_node -> [area] full_node list

  val add_action : t -> parent:uuid -> name:string -> description:string -> unit Lwt.t
  val add_project : t -> parent:uuid -> name:string -> description:string -> unit Lwt.t
  val add_area : t -> parent:uuid -> name:string -> description:string -> unit Lwt.t

  val delete : t -> uuid -> unit Lwt.t

  val set_name : t ->  [< area | action | project] full_node -> string -> unit Lwt.t
  val set_state : t -> uuid -> [< action | project | area] -> unit Lwt.t

  val process_tree : t -> node_view
  val work_tree : t -> node_view ReactiveData.RList.t
  val details : t -> uuid -> details
  val history : t -> (float * string) list React.S.t
end
