(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Making changes to the store. *)

open Ck_sigs

module Make(Git : Git_storage_s.S)
           (Clock : Ck_clock.S)
           (R : Ck_rev.S with type commit = Git.Commit.t) : sig
  type t
  type update_cb = R.t -> unit Lwt.t

  open R.Node.Types

  val make : on_update:update_cb Lwt.t -> Git.Branch.t -> t Lwt.t
  (** Manage updates to this branch.
   * Calls [on_update] after the branch has changed (either due to the methods below or because
   * the store has been modified by another process. *)

  val head : t -> R.t
  (** The current head. Usually the branch tip, but can be different if [fix_head] is used.
   * Also, this is the cached version of the last state of the head. It is the version
   * passed to [on_update] and might lag the real head slightly. *)

  val fix_head : t -> Git.Commit.t option -> unit Lwt.t
  (** Set [head] to the given commit and pause tracking our branch.
   * Pass [None] to return to tracking the branch's head.
   * Modifications made via [t] will automatically resume tracking, but changes
   * made by other means will be ignored. *)

  val fixed_head : t -> bool

  val branch_head : t -> Git.Commit.t
  (** The current tip of the branch (whatever the setting of [fix_head]) *)

  (** Functions for making updates all work in the same way.
   * 1. Make a new branch from the commit that produced the source item.
   * 2. Commit the change to that branch (this should always succeed).
   * 3. Merge the new branch to master.
   * 4. Call the [on_update] function.
   * When they return, on_update has completed for the new revision. *)

  val add : t -> ?uuid:Ck_id.t ->
    parent:[`Toplevel of R.t | `Node of [< area | project ]] ->
    (?parent:Ck_id.t -> ctime:float -> unit -> [ Ck_disk_node.Types.area | Ck_disk_node.Types.project | Ck_disk_node.Types.action]) ->
    Ck_id.t Lwt.t
  val add_contact : t -> base:R.t -> Ck_disk_node.Types.contact -> Ck_id.t Lwt.t
  val add_context : t -> ?uuid:Ck_id.t -> base:R.t -> Ck_disk_node.Types.context -> Ck_id.t Lwt.t
  val delete : t -> [< R.Node.generic] -> unit or_error Lwt.t
  val clear_conflicts : t -> [< R.Node.generic] -> unit Lwt.t

  val set_name : t -> [< R.Node.generic ] -> string -> unit Lwt.t
  val set_description : t -> [< R.Node.generic ] -> string -> unit Lwt.t
  val set_starred : t -> [< action | project] -> bool -> unit Lwt.t
  val set_action_state : t -> action -> [< action_state ] -> unit Lwt.t
  val set_repeat : t -> action -> Ck_time.repeat option -> unit Lwt.t
  val set_waiting_for : t -> action -> contact -> unit Lwt.t
  val set_project_state : t -> project -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t
  val set_context : t -> action -> context option -> unit Lwt.t
  val set_contact : t -> [< area | project | action] -> contact option -> unit Lwt.t

  val set_a_parent : t -> area -> area -> unit Lwt.t
  val set_pa_parent : t -> [< project | action] -> [< area | project] -> unit Lwt.t
  val remove_parent : t -> [< area | project | action] -> unit Lwt.t

  val convert_to_area : t -> project -> unit or_error Lwt.t
  val convert_to_project : t -> [< action | area] -> unit or_error Lwt.t
  val convert_to_action : t -> project -> unit or_error Lwt.t
end
