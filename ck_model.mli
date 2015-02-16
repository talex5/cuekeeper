(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make(I : Irmin.BASIC with type key = string list and type value = string) : sig
  include Ck_sigs.MODEL

  val make : (string -> I.t) -> t Lwt.t
end
