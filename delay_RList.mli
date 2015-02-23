(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* A list that delays delete events. This is useful if you want to display a fade-out animation
 * when things are removed. *)

module type CLOCK = sig
  val now : unit -> float
  val sleep : float -> unit Lwt.t
end

module Make (C : CLOCK) : sig
  val make : delay:float -> 'a ReactiveData.RList.t -> 'a ReactiveData.RList.t
end
