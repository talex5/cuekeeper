(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module Make(Git : Git_storage_s.S) : sig
  include REV with type commit = Git.Commit.t

  val make : Git.Commit.t -> t Lwt.t
  val disk_node : 'a Node.t -> 'a Ck_disk_node.t
end
