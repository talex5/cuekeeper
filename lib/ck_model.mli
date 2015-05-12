(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (C : Ck_clock.S) (Git : Git_storage_s.S) (G : sig type t end) (RPC : Cohttp_lwt.Client) : sig
  include Ck_model_s.MODEL with
    type gui_data = G.t

  val make : ?branch:string -> ?server:Uri.t -> Git.Repository.t -> t Lwt.t
end
