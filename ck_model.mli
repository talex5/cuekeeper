(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (C : Ck_clock.S) (I : Irmin.BASIC with type key = string list and type value = string) : sig
  include Ck_model_s.MODEL

  val make : (string -> I.t) -> t Lwt.t
end
