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

module Make (C : Ck_clock.S) (M : TREE_MODEL) (G : GUI_DATA) : sig
  type t

  module Widget : sig
    (** An object visible on the screen. *)
    type t
    val item : t ->
      [ `Item of M.Item.generic React.S.t
      | `Group of string]
    val children : t -> t ReactiveData.RList.t
    val state : t -> Slow_set.state React.S.t
    val gui_data : t -> G.t option ref
    val unique : t -> bool
    (** Unique items occur at most once in the tree (and are often leaves).
     * Non-unique items are used for grouping, and are typically rendered as headings. *)
  end

  val make : M.t M.Child_map.t -> t

  (** Walk the new tree, updating the current RLists, creating new ones where necessary.
   * Widgets that are no longer present are removed from the output lists and [on_remove]
   * is called on each one. This is useful to provide one last update showing why the
   * item was removed as it fades out.
   *)
  val update : t -> on_remove:(M.Item.generic -> M.Item.generic option) -> M.t M.Child_map.t -> unit

  val widgets : t -> Widget.t ReactiveData.RList.t
end
