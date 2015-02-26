(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std

type action_details = {
  astarred : bool with default(false);
  astate : [ `Next | `Waiting | `Future | `Done ]
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type action = [`Action of action_details]
type project = [`Project of project_details]
type area = [`Area]

module type DISK_NODE = sig
  type +'a t

  val parent : 'a t -> Ck_id.t
  val name : 'a t -> string
  val description : 'a t -> string
  val ctime : 'a t -> float
  val details : 'a t -> 'a

  val with_name : 'a t -> string -> 'a t
  val with_details : _ t -> 'a -> 'a t
end

module type MODEL = sig
  type t
  type 'a full_node

  module View : sig
    type t = {
      uuid : Ck_id.t;
      init_node_type : [ area | project | action ]; (* Hack for "signal value undefined yet" *)
      node_type : [ area | project | action | `Deleted ] React.S.t;
      ctime : float;
      name : string React.S.t;
      description : string React.S.t;
      child_views : t ReactiveData.RList.t;
      state : int Slow_set.state React.S.t;
    }
  end

  val root : t -> [area] full_node React.S.t
  val is_root : Ck_id.t -> bool

  val all_areas_and_projects : t -> (string * [> area | project] full_node) list

  val uuid : _ full_node -> Ck_id.t

  val add_action : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_project : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_area : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t

  val delete : t -> Ck_id.t -> unit Lwt.t

  val set_name : t ->  Ck_id.t -> string -> unit Lwt.t
  val set_details : t -> Ck_id.t -> [< action | project | area] -> unit Lwt.t
  val set_starred : t -> Ck_id.t -> bool -> unit Lwt.t

  val process_tree : t -> View.t
  val work_tree : t -> View.t ReactiveData.RList.t
  val details : t -> Ck_id.t -> View.t
  val history : t -> (float * string) list React.S.t
end
