(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module Make (C : Ck_clock.S) (Git : Git_storage_s.S) (G : sig type t end) : sig
  include Ck_model_s.MODEL with
    type gui_data = G.t

  val make : ?branch:string -> Git.Repository.t -> t Lwt.t
  val sync : t -> from:Git.Commit.t -> unit or_error Lwt.t
end
