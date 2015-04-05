(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

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
  let compare a b = (* Newest first *)
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
    val read_exn : t -> path -> string Lwt.t
    val update : t -> path -> string -> unit Lwt.t
    val remove : t -> path -> unit Lwt.t
    val mem : t -> path -> bool Lwt.t
  end

  module Commit : sig
    type t

    val checkout : t -> Staging.t Lwt.t
    val commit : ?parents:t list -> Staging.t -> msg:string -> t Lwt.t
    val merge : t -> t -> [ `Conflict of string | `Ok of t ] Lwt.t
    val equal : t -> t -> bool
    val history : ?depth:int -> t -> Log_entry.t Log_entry_map.t Lwt.t
  end

  module Branch : sig
    type t

    val head : t -> Commit.t option React.S.t
    val fast_forward_to : t -> Commit.t -> [ `Ok | `Not_fast_forward ] Lwt.t
  end

  module Repository : sig
    type t

    val branch : t -> if_new:(Commit.t Lwt.t Lazy.t) -> string -> Branch.t Lwt.t
    (** Get the named branch.
     * If the branch does not exist yet, [if_new] is called to get the initial commit. *)

    val commit : t -> Irmin.Hash.SHA1.t -> Commit.t option Lwt.t
    (** Look up a commit by its hash. *)

    val empty : t -> Staging.t Lwt.t
    (** Create an empty checkout with no parent. *)
  end
end
