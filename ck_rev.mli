(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs


module Make(I : Irmin.BASIC with type key = string list and type value = string) : sig
  include REV with type V.db = I.t

  val make : (string -> I.t) -> t Lwt.t
  val disk_node : 'a Node.t -> 'a Ck_disk_node.t
end
