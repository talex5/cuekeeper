(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

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

  module View : sig
    type t = {
      uuid : Ck_id.t;
      init_node_type : [ area | project | action ]; (* Hack for "signal value undefined yet" *)
      node_type : [ area | project | action | `Deleted ] React.S.t;
      ctime : float;
      name : string React.S.t;
      description : string React.S.t;
      child_views : t ReactiveData.RList.t;
      state : Slow_set.state React.S.t;
    }
  end

  val root : t -> [area] full_node React.S.t
  val is_root : Ck_id.t -> bool

  val all_areas_and_projects : t -> (string * [> area | project] full_node) list

  val name : _ full_node -> string
  val uuid : _ full_node -> Ck_id.t

  val actions : [< area | project] full_node -> [action] full_node list
  val projects : [< area | project] full_node -> [project] full_node list
  val areas : [area] full_node -> [area] full_node list

  val add_action : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_project : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_area : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t

  val delete : t -> Ck_id.t -> unit Lwt.t

  val set_name : t ->  Ck_id.t -> string -> unit Lwt.t
  val set_state : t -> Ck_id.t -> [< action | project | area] -> unit Lwt.t

  val process_tree : t -> View.t
  val work_tree : t -> View.t ReactiveData.RList.t
  val details : t -> Ck_id.t -> View.t
  val history : t -> (float * string) list React.S.t
end
