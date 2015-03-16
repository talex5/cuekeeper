(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type gui_data

  module Item : sig
    include DISK_NODE
    val uuid : [< generic] -> Ck_id.t
    val is_due : Types.action_node -> bool
    (** Whether a [`Waiting_until] action is due. *)
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

  val add_action : t -> state:Ck_id.t action_state -> ?parent:[< area | project] ->
                   name:string -> description:string -> [area | project | action] option Lwt.t
  val add_project : t -> ?parent:[< area | project] -> name:string -> description:string -> [area | project | action] option Lwt.t
  val add_area : t -> ?parent:[< area] -> name:string -> description:string -> [area | project | action] option Lwt.t
  val add_contact : t -> name:string -> [> contact] option Lwt.t

  val add_child : t -> [< area | project] -> string -> [area | project | action] option Lwt.t

  val delete : t -> [< Item.generic] -> unit or_error Lwt.t

  val set_name : t ->  [< Item.generic] -> string -> unit Lwt.t
  val set_description : t ->  [< Item.generic] -> string -> unit Lwt.t
  val set_starred : t -> [< project | action] -> bool -> unit Lwt.t
  val set_action_state : t -> action_node -> [< contact_node action_state] -> unit Lwt.t
  val set_project_state : t -> project_node -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t

  val convert_to_area : t -> project_node -> unit or_error Lwt.t
  val convert_to_project : t -> [< action | area] -> unit or_error Lwt.t
  val convert_to_action : t -> project_node -> unit or_error Lwt.t

  type candidate

  val candidate_parents_for : t -> [< area | project | action] -> candidate list
  (** Get the possible new parents for an item. *)

  val candidate_contacts_for : t -> action -> candidate list
  (** Get the possible contacts for an action. *)

  val candidate_label : candidate -> string
  val choose_candidate : candidate -> unit Lwt.t

  val log : t -> Git_storage_s.Log_entry.t Slow_set.item ReactiveData.RList.t
  val fix_head : t -> Git_storage_s.Log_entry.t option -> unit Lwt.t
  val fixed_head : t -> Git_storage_s.Log_entry.t option React.S.t

  val set_mode : t -> [ `Process | `Work | `Contact | `Review | `Schedule ] -> unit
  val tree : t -> [ `Process of Widget.t ReactiveData.RList.t
                  | `Work of Widget.t ReactiveData.RList.t
                  | `Contact of Widget.t ReactiveData.RList.t
                  | `Review of Widget.t ReactiveData.RList.t
                  | `Schedule of Widget.t ReactiveData.RList.t] React.S.t

  val details : t -> [< Item.generic] -> details

  val alert : t -> bool React.S.t
  (** Alert the user that action is required (the Work view will show what) *)
end
