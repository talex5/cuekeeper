(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* A list that delays delete events. This is useful if you want to display a fade-out animation
 * when things are removed. *)

type set_state

type 'a item = {
  set_state : set_state;
  state : [ `New | `Current | `Removed ] React.S.t;
  data : 'a;
}

val fixed : 'a -> 'a item

module Make (C : Ck_clock.S) : sig
  val make : delay:float -> 'a ReactiveData.RList.t -> 'a item ReactiveData.RList.t
end
