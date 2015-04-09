(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module Make(Git : Git_storage_s.S) : sig
  include REV with type commit = Git.Commit.t
  open Node.Types

  val make : time:Ck_time.user_date -> Git.Commit.t -> t Lwt.t
  val disk_node : [< Node.generic] -> Ck_disk_node.generic
  val apa_node : [< area | project | action] ->
    [ Ck_disk_node.Types.area | Ck_disk_node.Types.project | Ck_disk_node.Types.action ]

  val action_node : action -> Ck_disk_node.Types.action
  val project_node : project -> Ck_disk_node.Types.project
  val area_node : area -> Ck_disk_node.Types.area
end
