(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* A list that delays delete events. This is useful if you want to display a fade-out animation
 * when things are removed. *)

module type CLOCK = sig
  val now : unit -> float
  val sleep : float -> unit Lwt.t
end

type set_state

type 'a item = {
  set_state : set_state;
  state : [ `Current | `Removed ] React.S.t;
  data : 'a;
}

val fixed : 'a -> 'a item

module Make (C : CLOCK) : sig
  val make : delay:float -> 'a ReactiveData.RList.t -> 'a item ReactiveData.RList.t
end
