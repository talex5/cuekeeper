(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Client-side code for syncing with a remote server. *)

open Ck_sigs

val tracking_branch : Git_storage_s.branch_name

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (RPC : RPC) : sig
  type t

  val make :
    master:Git.Branch.t ->
    server_branch:Git.Branch.t ->
    merge_from:(Git.Commit.t -> unit or_error Lwt.t) ->
    Uri.t ->
    t
  (** Create a client for the server at the given URL.
   * Syncing will fetch changes into [server_branch] and then merge them into [master]
   * using [merge_from], before pushing [master] to the server. *)

  val fetch : base:Uri.t -> server_branch:Git.Branch.t -> Git.Commit.t option or_error_or_cancelled Lwt.t
  (* Fetch the current server head and store in [server_branch].
   * Returns the [Commit.t] for the server's head.
   * Exposed to allow initialising the repository from the server on first use. *)

  val sync : t -> unit or_error_or_cancelled Lwt.t
  (** Sync with server. *)

  val sync_in_progress : t -> bool React.S.t
  (** True while we are syncing with the remote server. *)
end
