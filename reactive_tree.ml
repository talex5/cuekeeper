(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

(* Note on GC:
 *
 * Signals created using S.map, bind, etc, register a callback with the source
 * signal. We need to make sure this callback gets removed, otherwise we will
 * leak memory and waste time on pointless updates.
 *
 * In native code, the producer has a weak ref to the callback and will GC it
 * eventually (although this makes everything non-deterministic and still wastes
 * time before the GC occurs).
 *
 * In JavaScript, there are no weak refs and we always leak unless we call
 * [S.stop ~strong:true]. However, this not only unregisters the callback from
 * the source, but recursively stops the source too if it has no other handlers
 * (which we don't know in general).
 *
 * Therefore, we avoid these functions here and set the signal states manually.
 * Our source signal only has a reference to the tree as a whole. When a widget
 * is removed, it can therefore be GC'd (along with anything that depends on it).
 *)

module Make (C : Ck_clock.S) (M : TREE_MODEL) = struct
  module Slow = Slow_set.Make(C)(M.Sort_key)(M.Child_map)

  module W = struct
    type t = {
      id : M.Id_map.key;
      item : M.Item.generic React.S.t;
      set_item : M.Item.generic -> unit;
      children : (t, M.move_data) Slow_set.item ReactiveData.RList.t;
      set_child_widgets : ?step:React.step -> t M.Child_map.t -> unit;
    }

    let id t = t.id
    let item t = t.item
    let children t = t.children

    let equal a b = a.id = b.id
    (* Delta wants to know when to send updates. In fact, widgets never update,
     * so we could just return true here. *)
  end

  module Widget = struct
    type t = (W.t, M.move_data) Slow_set.item

    let id t = W.id (Slow_set.data t)
    let item t = W.item (Slow_set.data t)
    let children t = W.children (Slow_set.data t)
    let state = Slow_set.state

    let equal a b =
      W.equal (Slow_set.data a) (Slow_set.data b)
  end

  module Delta = Delta_RList.Make(M.Sort_key)(Widget)(M.Child_map)

  let rec make_widget ~widgets node : W.t =
    (* Printf.printf "make_widget(%s)\n" (M.Item.show (M.item node)); *)
    let children, set_child_widgets = make_widgets ~widgets (M.children node) in
    let item, set_item = React.S.create ~eq:M.Item.equal (M.item node) in
    let id = M.id node in
    let widget = { W.
      id;
      item;
      set_item;
      children;
      set_child_widgets;
    } in
    (* todo: check for duplicates? *)
    widgets := !widgets |> M.Id_map.add id widget;
    widget
  and make_widgets ~widgets nodes =
    let init_children = nodes |> M.Child_map.map (make_widget ~widgets) in
    let child_widgets, set_child_widgets =
      React.S.create ~eq:(M.Child_map.equal W.equal) init_children in
    let children = child_widgets
      |> Slow.make
          ~delay:1.0
          ~init:init_children
          ~eq:W.equal
      |> Delta.make in
    (children, set_child_widgets)

  type t = {
    widgets : W.t M.Id_map.t ref;
    root_widgets : Widget.t ReactiveData.RList.t;
    set_root_widgets : ?step:React.step -> W.t M.Child_map.t -> unit;
  }

  let add_widget t node =
    make_widget ~widgets:t.widgets node

  let rec update t ~old_widgets =
    M.Child_map.map (fun node ->
      let id = M.id node in
      try
        let existing = M.Id_map.find id old_widgets in
        M.children node |> update t ~old_widgets |> existing.W.set_child_widgets;
        t.widgets := !(t.widgets) |> M.Id_map.add id existing;
        existing.W.set_item (M.item node);
        existing
      with Not_found -> add_widget t node
    )

  let make nodes =
    let widgets = ref M.Id_map.empty in
    let root_widgets, set_root_widgets = make_widgets ~widgets nodes in
    {
      widgets;
      root_widgets;
      set_root_widgets;
    }

  let update t nodes =
    (* print_endline "\n== update ==\n"; *)
    let old_widgets = !(t.widgets) in
    t.widgets := M.Id_map.empty;
    update t ~old_widgets nodes |> t.set_root_widgets

    (*
      old_widgets |> M.Id_map.iter (fun k old ->
        if not (M.Id_map.mem k !(t.widgets)) then (
          Printf.printf "Removed unused widget '%s'\n" (M.Item.show (React.S.value old.W.item))
        )
      )
    *)

  let widgets t = t.root_widgets
end
