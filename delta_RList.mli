(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(* Generates an RList from a signal of a set.
 * Unlike RList.make_from_s, this generates individual add and remove events
 * rather than replacing the whole list every time the signal changes. *)

module Make (Item : Set.OrderedType) (Input : Set.S with type elt = Item.t) : sig
  (* If this can be called from a React update, you must provide the [init] field,
   * since [React.S.value input] won't be ready yet. *)
  val make : ?init:Input.t -> Input.t React.S.t -> Item.t ReactiveData.RList.t
end
