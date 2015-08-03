(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type branch_name = string
type bundle = string
type path = string list

module Log_entry = struct
  module Id = struct
    type t = Irmin.Hash.SHA1.t
    let compare  = compare
  end
  type t = {
    id : Irmin.Hash.SHA1.t;
    rank : int;
    date : float;
    msg : string list;
  }
  let compare b a = (* Newest first *)
    compare a.rank b.rank
  let id x = x.id
  let show x = String.concat "\n" x.msg
  let equal a b =
    a.id = b.id
end

module Log_entry_map = Map.Make(Log_entry)

module type S = sig
  module Staging : sig
    type t

    val list : t -> path -> path list Lwt.t
    val read : t -> path -> string option Lwt.t
    val read_exn : t -> path -> string Lwt.t
    val update : t -> path -> string -> unit Lwt.t
    val remove : t -> path -> unit Lwt.t
    val mem : t -> path -> bool Lwt.t
  end

  module Commit : sig
    type t

    val checkout : t -> Staging.t Lwt.t
    val commit : ?parents:t list -> Staging.t -> msg:string list -> t Lwt.t
    val merge : t -> t -> [ `Conflict of string | `Ok of t ] Lwt.t
    val equal : t -> t -> bool
    val history : ?depth:int -> t -> Log_entry.t Log_entry_map.t Lwt.t
    val export_tar : t -> string Lwt.t
    val parents : t -> t list Lwt.t
    val task : t -> Irmin.Task.t Lwt.t
    val lcas : t -> t -> t list Lwt.t
    (** Find the least common ancestor(s) of two commits.
     * This is used as the base when doing a 3-way merge. *)

    val bundle : tracking_branch:branch_name -> t -> bundle option Lwt.t
    (** Exports the given commit with full history, excluding anything
     * already in [tracking_branch]. The resulting bundle can be imported
     * into the remote repository that [tracking_branch] tracks.
     * If there is nothing to export, returns None. *)
  end

  module Branch : sig
    type t

    val head : t -> Commit.t option React.S.t
    val fast_forward_to : t -> Commit.t -> [ `Ok | `Not_fast_forward ] Lwt.t

    val release : t -> unit Lwt.t
    (** Stop watching this branch for updates ([head] will no longer update and
     * [t] should not be used again). *)
  end

  module Repository : sig
    type t

    val branch : t -> if_new:(Commit.t Lwt.t Lazy.t) -> branch_name -> Branch.t Lwt.t
    (** Get the named branch.
     * If the branch does not exist yet, [if_new] is called to get the initial commit. *)

    val branch_head : t -> branch_name -> Irmin.Hash.SHA1.t option Lwt.t
    (** Check the current head of a branch. None if the branch doesn't exist. *)

    val force_branch : t -> branch_name -> Commit.t option -> unit Lwt.t
    (** Set the head of the named branch to point at this commit.
     * If None, the branch is deleted. *)

    val commit : t -> Irmin.Hash.SHA1.t -> Commit.t option Lwt.t
    (** Look up a commit by its hash. *)

    val empty : t -> Staging.t Lwt.t
    (** Create an empty checkout with no parent. *)

    val fetch_bundle : t -> tracking_branch:branch_name -> bundle -> Commit.t Ck_sigs.or_error Lwt.t
    (** Import the contents of the bundle into the repository, updating [tracking_branch] to
     * point to the bundle's head (even if not a fast-forward). Returns the new head.  *)
  end
end
