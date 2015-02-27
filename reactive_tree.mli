(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Converts a signal of a tree to a tree of signals.
 *
 * In particular, it gives a persistent identity to nodes, making animations
 * possible.
 *
 * The output signals will be automatically GC'd when the node is removed
 * from the tree, even on platform without weak refs.
 *
 * Updates are done by calling [update] rather than by passing a signal to
 * [make] to avoid resource leaks.
 *)

open Ck_sigs

module Make (C : Ck_clock.S) (M : TREE_MODEL) : sig
  type t

  module Widget : sig
    (** An object visible on the screen. *)
    type t
    val id : t -> Ck_id.t
    val item : t -> M.Item.t React.S.t
    val children : t -> t ReactiveData.RList.t
    val state : t -> M.move_data Slow_set.state React.S.t
  end

  val make : M.t M.Child_map.t -> t
  val update : t -> M.t M.Child_map.t -> unit
  val widgets : t -> Widget.t ReactiveData.RList.t
end
