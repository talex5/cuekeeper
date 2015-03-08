(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type path = string list
type log_entry = {
  date : float;
  msg : string list;
}

module type S = sig
  module Staging : sig
    type t

    val empty : unit -> t Lwt.t
    val list : t -> path -> path list Lwt.t
    val read_exn : t -> path -> string Lwt.t
    val update : t -> path -> string -> unit Lwt.t
    val remove : t -> path -> unit Lwt.t
  end

  module Commit : sig
    type t

    val checkout : t -> Staging.t Lwt.t
    val commit : Staging.t -> msg:string -> t Lwt.t
    val merge : t -> t -> [ `Conflict of string | `Ok of t ] Lwt.t
    val equal : t -> t -> bool
    val history : ?depth:int -> t -> log_entry list Lwt.t
  end

  module Branch : sig
    type t

    val head : t -> Commit.t option React.S.t
    val fast_forward_to : t -> Commit.t -> [ `Ok | `Not_fast_forward ] Lwt.t
  end

  module Repository : sig
    type t

    val branch : t -> if_new:(Staging.t -> unit Lwt.t) -> string -> Branch.t Lwt.t
    (** Get the named branch.
     * Hack: if the branch does not exist yet, [if_new] is called to get
     * the initial contents, which are then committed. This is because Irmin doesn't
     * allow commits with no parent unless they're on a named branch. *)
  end
end
