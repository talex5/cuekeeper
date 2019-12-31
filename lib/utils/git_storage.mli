(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

(** A Git-like storage abstraction over Irmin. *)

module Make (I : Irmin.S
             with type key = string list
              and type contents = string
              and type Commit.Hash.t = Irmin.Hash.SHA1.t
              and type branch = string
              and type step = string) : sig
  include Git_storage_s.S

  val make : I.Repo.t -> (string -> Irmin.Info.t) -> Repository.t
end
