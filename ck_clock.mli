(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module type S = sig
  val now : unit -> float
  val async : (unit -> unit Lwt.t) -> unit
  val sleep : float -> unit Lwt.t
end
