(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Merging two branches together. *)

module Make (Git : Git_storage_s.S) (R : Ck_rev.S with type commit = Git.Commit.t) : sig
  (* [merge ?base ~theirs ours] merges changes from [base] to [ours] into [theirs] and
   * returns the resulting merge commit. *)
  val merge : ?base:Git.Commit.t -> theirs:Git.Commit.t -> Git.Commit.t ->
    [`Ok of Git.Commit.t | `Nothing_to_do] Lwt.t

  (** [revert ~master log_entry] returns a new commit on [master] which reverts the changes in [log_entry]. *)
  val revert : repo:Git.Repository.t -> master:Git.Commit.t -> Git_storage_s.Log_entry.t ->
    [`Ok of Git.Commit.t | `Nothing_to_do | `Error of string] Lwt.t
end
