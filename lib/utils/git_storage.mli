(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

(** A Git-like storage abstraction over Irmin. *)

module Make (I : Irmin.S with type key = string list and type value = string and
            type commit_id = Irmin.Hash.SHA1.t and type branch_id = string) : sig
  include Git_storage_s.S

  val make : I.Repo.t -> (string -> Irmin.task) -> Repository.t Lwt.t
end
