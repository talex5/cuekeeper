(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type gui_data

  module Item : sig
    include DISK_NODE
    val uuid : [< generic] -> Ck_id.t
  end

  open Item.Types

  module Widget : sig
    (** An object visible on the screen. *)
    type t

    val item : t -> [
      | `Item of [ area | project | action | contact ] React.S.t
      | `Group of string
    ]
    val children : t -> t ReactiveData.RList.t
    val state : t -> Slow_set.state React.S.t
    val gui_data : t -> gui_data option ref
  end

  type details = {
    details_item : [ area | project | action | contact ] option React.S.t;
    details_parent : [ area | project | action ] option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  val add_action : t -> ?parent:[< area | project] -> name:string -> description:string -> [area | project | action] option Lwt.t
  val add_project : t -> ?parent:[< area | project] -> name:string -> description:string -> [area | project | action] option Lwt.t
  val add_area : t -> ?parent:[< area] -> name:string -> description:string -> [area | project | action] option Lwt.t
  val add_contact : t -> name:string -> [> contact] option Lwt.t

  val add_child : t -> [< area | project] -> string -> [area | project | action] option Lwt.t

  val delete : t -> [< Item.generic] -> unit or_error Lwt.t

  val set_name : t ->  [< Item.generic] -> string -> unit Lwt.t
  val set_starred : t -> [< project | action] -> bool -> unit Lwt.t
  val set_action_state : t -> action_node -> [ `Next | `Waiting | `Future | `Done ] -> unit Lwt.t
  val set_project_state : t -> project_node -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t

  val convert_to_area : t -> project_node -> unit or_error Lwt.t
  val convert_to_project : t -> [< action | area] -> unit or_error Lwt.t
  val convert_to_action : t -> project_node -> unit or_error Lwt.t

  type candidate_parent

  val candidate_parents_for : t -> [< area | project | action] -> candidate_parent list
  (** Get the possible new parents for an item. *)

  val candidate_label : candidate_parent -> string
  val set_parent : candidate_parent -> unit Lwt.t

  val set_mode : t -> [ `Process | `Work | `Sync | `Contact | `Review | `Schedule ] -> unit
  val tree : t -> [ `Process of Widget.t ReactiveData.RList.t
                  | `Work of Widget.t ReactiveData.RList.t
                  | `Sync of Git_storage_s.log_entry list React.S.t
                  | `Contact of Widget.t ReactiveData.RList.t
                  | `Review of Widget.t ReactiveData.RList.t
                  | `Schedule of unit ] React.S.t

  val details : t -> [< Item.generic] -> details
end
