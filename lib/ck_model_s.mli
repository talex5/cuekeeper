(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type gui_data

  type review_mode = [ `Done | `Waiting | `Future | `Areas | `Everything ]

  module Item : sig
    include DISK_NODE
    open Types

    val uuid : [< generic] -> Ck_id.t

    val is_due : Types.action -> bool
    (** Whether a [`Waiting_until] action is due. *)

    val contact_node : [< area | project | action] -> contact option
  end

  open Item.Types

  module Widget : sig
    (** An object visible on the screen. *)
    type t

    val item : t -> [
      | `Item of [ area | project | action | contact | context ] React.S.t
      | `Group of string
    ]
    val children : t -> t ReactiveData.RList.t
    val state : t -> Slow_set.state React.S.t
    val gui_data : t -> gui_data option ref
    val unique : t -> bool
    (** Unique items occur at most once in the tree (and are often leaves).
     * Non-unique items are used for grouping, and are typically rendered as headings. *)

    type adder
    val adder : t -> adder option
  end

  type details = {
    details_item : [ area | project | action | contact | context ] option React.S.t;
    details_parent : [ area | project | action ] option React.S.t;
    details_context : context option option React.S.t;
    details_contact : contact option React.S.t option;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  val add_action : t -> state:action_state -> ?context:context -> ?contact:contact -> ?parent:[< area | project] ->
                   name:string -> ?description:string -> unit -> [area | project | action] option Lwt.t
  val add_project : t -> ?state:project_state -> ?parent:[< area | project] -> name:string -> ?description:string -> unit -> [area | project | action] option Lwt.t
  val add_area : t -> ?parent:[< area] -> name:string -> ?description:string -> unit -> [area | project | action] option Lwt.t

  val add_contact : t -> name:string -> unit -> [> contact] option Lwt.t
  val set_contact : t -> [< area | project | action] -> contact option -> unit or_error Lwt.t

  val add_context : t -> name:string -> unit -> [> context] option Lwt.t
  val set_context : t -> action -> context -> unit or_error Lwt.t

  val add_child : t -> [< area | project | context] -> string -> [area | project | action] option Lwt.t
  val apply_adder : t -> Widget.adder -> string -> Item.generic option Lwt.t

  val clear_conflicts : t -> [< Item.generic] -> unit Lwt.t
  val delete : t -> [< Item.generic] -> unit or_error Lwt.t
  val delete_done : t -> unit Lwt.t

  val set_name : t ->  [< Item.generic] -> string -> unit Lwt.t
  val set_description : t ->  [< Item.generic] -> string -> unit Lwt.t
  val set_starred : t -> [< project | action] -> bool -> unit Lwt.t
  val set_action_state : t -> action -> [< action_state] -> unit Lwt.t
  val set_project_state : t -> project -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t
  val set_repeat : t -> action -> Ck_time.repeat option -> unit Lwt.t

  val convert_to_area : t -> project -> unit or_error Lwt.t
  val convert_to_project : t -> [< action | area] -> unit or_error Lwt.t
  val convert_to_action : t -> project -> unit or_error Lwt.t

  type candidate

  val candidate_parents_for : t -> [< area | project | action] -> candidate list
  (** Get the possible new parents for an item. *)

  val candidate_contacts_for : t -> [< area | project | action] -> candidate list
  (** Get the possible contacts for an action. *)

  val candidate_contexts_for : t -> action -> candidate list
  (** Get the possible contexts for an action. *)

  val candidate_label : candidate -> string
  val choose_candidate : candidate -> unit Lwt.t

  val enable_log : t -> Git_storage_s.Log_entry.t Slow_set.item ReactiveData.RList.t Lwt.t
  val disable_log : t -> unit
  val revert : t -> Git_storage_s.Log_entry.t -> unit or_error Lwt.t

  val fix_head : t -> Git_storage_s.Log_entry.t option -> unit Lwt.t
  val fixed_head : t -> Git_storage_s.Log_entry.t option React.S.t

  type filter = (area * bool) list

  val set_mode : t -> [ `Process | `Work | `Contact | `Review | `Schedule ] -> unit
  val tree : t -> [ `Process of Widget.t ReactiveData.RList.t
                  | `Work of filter React.S.t * Widget.t ReactiveData.RList.t
                  | `Contact of Widget.t ReactiveData.RList.t
                  | `Review of review_mode * Widget.t ReactiveData.RList.t
                  | `Schedule of Widget.t ReactiveData.RList.t] React.S.t

  val set_review_mode : t -> review_mode -> unit

  val details : t -> [< Item.generic] -> details

  val alert : t -> bool React.S.t
  (** Alert the user that action is required (the Work view will show what) *)

  val export_tar : t -> string Lwt.t
  (** Export the current state as a tar file *)

  val search : t -> n:int -> (Item.generic -> 'a option) -> 'a Ck_utils.M.t
  (** [search t ~n test] finds up to [n] non-None results from [test item], which it calls
   * for all areas, projects, actions, contacts and contexts (until it has enough results). *)

  val set_hidden : t -> area -> bool -> unit
  (** Set whether to hide this top-level area in the work tab. *)

  type server

  val server : t -> server option
  (** The server to sync to, if any. *)

  val sync : server -> unit or_error Lwt.t
  (** Sync with server. *)
end
