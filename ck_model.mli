(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (C : Ck_clock.S) (I : Irmin.BASIC with type key = string list and type value = string)(G : sig type t end) : sig
  include Ck_model_s.MODEL with
    type gui_data = G.t

  val make : Irmin.config -> (string -> Irmin.task) -> t Lwt.t
end
