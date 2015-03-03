(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Making changes to the store. *)

open Ck_sigs

module Make(I : Irmin.BASIC with type key = string list and type value = string)
           (R : sig
             include REV with type V.db = I.t
             val disk_node : 'a Node.t -> 'a Ck_disk_node.t
           end) : sig
  type t
  type update_cb = Irmin.Hash.SHA1.t -> unit Lwt.t

  val make : on_update:update_cb Lwt.t -> (string -> I.t) -> t Lwt.t
  (** Manage updates to this branch.
   * Calls [on_update] after the branch has changed (either due to the methods below or because
   * the store has been modified by another process. *)

  val head : t -> Irmin.Hash.SHA1.t
  (** The current tip of the branch. *)

  (** Functions for making updates all work in the same way.
   * 1. Make a new branch from the commit that produced the source item.
   * 2. Commit the change to that branch (this should always succeed).
   * 3. Merge the new branch to master.
   * 4. Call the [on_update] function.
   * When they return, on_update has completed for the new revision. *)

  val add : t -> ?uuid:Ck_id.t -> [< action | project | area] ->
    parent:[`Toplevel of R.t | `Node of R.Node.generic] -> name:string -> description:string -> Ck_id.t Lwt.t
  val delete : t -> [< action | project | area] R.Node.t -> unit Lwt.t

  val set_name : t -> [< action | project | area] R.Node.t -> string -> unit Lwt.t
  val set_starred : t -> [< action | project] R.Node.t -> bool -> unit Lwt.t
  val set_action_state : t -> [action] R.Node.t -> [ `Next | `Waiting | `Future | `Done ] -> unit Lwt.t
  val set_project_state : t -> [project] R.Node.t -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t
end
