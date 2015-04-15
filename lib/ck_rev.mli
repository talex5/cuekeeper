(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module type S = sig
  include REV
  open Node.Types

  val make : time:Ck_time.user_date -> commit -> t Lwt.t
  val disk_node : [< Node.generic] -> Ck_disk_node.Types.node
  val apa_node : [< area | project | action] -> Ck_disk_node.Types.apa_node

  val action_node : action -> Ck_disk_node.Types.action_node
  val project_node : project -> Ck_disk_node.Types.project_node
  val area_node : area -> Ck_disk_node.Types.area_node
  val context_node : context -> Ck_disk_node.Types.context_node
  val contact_node : contact -> Ck_disk_node.Types.contact_node
end

module Make(Git : Git_storage_s.S) : S with type commit = Git.Commit.t
