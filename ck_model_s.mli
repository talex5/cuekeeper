(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type 'a full_node

  module Item : sig
    include DISK_NODE
    val disk_node : 'a t -> 'a Ck_disk_node.t   (* XXX *)
    val uuid : [< area | project | action] t -> Ck_id.t
    val ty : [< action | project | area] t ->
      [ `Action of [> action] t
      | `Project of [> project] t
      | `Area of [> area] t ]
  end

  module Widget : sig
    (** An object visible on the screen. *)
    type t
    val item : t -> [ area | project | action] Item.t React.S.t
    val children : t -> t ReactiveData.RList.t
    val state : t -> int Slow_set.state React.S.t
  end

  type details = {
    details_item : [ area | project | action ] Item.t option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  val root : t -> [area] full_node React.S.t
  val is_root : Ck_id.t -> bool

(*   val all_areas_and_projects : t -> (string * [> area | project] full_node) list *)

  val add_action : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_project : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_area : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t

  val delete : t -> [< action | project | area] Item.t -> unit Lwt.t

  val set_name : t ->  Ck_id.t -> string -> unit Lwt.t
  val set_details : t -> Ck_id.t -> [< action | project | area] -> unit Lwt.t
  val set_starred : t -> [< project | action] Item.t -> bool -> unit Lwt.t
  val set_action_state : t -> [action] Item.t -> [ `Next | `Waiting | `Future | `Done ] -> unit Lwt.t
  val set_project_state : t -> [project] Item.t -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t

  val process_tree : t -> Widget.t ReactiveData.RList.t
  val work_tree : t -> Widget.t ReactiveData.RList.t
  val details : t -> Ck_id.t -> details
  val history : t -> (float * string) list React.S.t
end
