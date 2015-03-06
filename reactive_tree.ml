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

module Make (C : Ck_clock.S) (M : TREE_MODEL) (G : GUI_DATA) = struct
  module Slow = Slow_set.Make(C)(M.Sort_key)(M.Child_map)

  type id =
    | Root_id
    | Item_id of Ck_id.t
    | Group_id of M.group_id * id     (* Path of group relative to ancestor with UUID *)

  module Id_map = Map.Make(struct
    type t = id
    let compare = compare
  end)

  module W = struct
    type item =
      [ `Item of M.Item.generic React.S.t * (?step:React.step -> M.Item.generic -> unit)
      | `Group of M.group_id ]

    type t = {
      item : item;
      children : t Slow_set.item ReactiveData.RList.t;
      set_child_widgets : ?step:React.step -> t M.Child_map.t -> unit;
      gui_data : G.t option ref;
    }

    let item t =
      match t.item with
      | `Item (item, _) -> `Item item
      | `Group gid -> `Group (M.group_label gid)

    let children t = t.children

    let equal a b =
      match a.item, b.item with
      | `Item _, `Item _ ->
          (* Delta wants to know when to send updates. In fact, widgets never update,
           * so we can just return true here. *)
          true
      | `Group a, `Group b -> a = b
      | _ -> false
  end

  module Widget = struct
    type t = W.t Slow_set.item

    let item t = W.item (Slow_set.data t)
    let children t = W.children (Slow_set.data t)
    let state = Slow_set.state
    let gui_data t = (Slow_set.data t).W.gui_data

    let equal a b =
      W.equal (Slow_set.data a) (Slow_set.data b)
  end

  module Delta = Delta_RList.Make(M.Sort_key)(Widget)(M.Child_map)

  let rec make_widget ~widgets ~get_child ~parent_id node : W.t =
    (* Printf.printf "make_widget(%s)\n" (M.Item.show (M.item node)); *)
    let item, id =
      match M.item node with
      | `Item (id, item) ->
          let item, set_item = React.S.create ~eq:M.Item.equal item in
          `Item (item, set_item), Item_id id
      | `Group label as g -> g, (Group_id (label, parent_id)) in
    let children, set_child_widgets =
      make_widgets ~get_child ~parent_id:id (M.children node) in
    let widget = { W.
      item;
      children;
      set_child_widgets;
      gui_data = ref None;
    } in
    (* todo: check for duplicates? *)
    widgets := !widgets |> Id_map.add id widget;
    widget
  and make_widgets ~get_child ~parent_id nodes =
    let init_children = nodes |> M.Child_map.map (get_child ~parent_id) in
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
    widgets : W.t Id_map.t ref;
    root_widgets : Widget.t ReactiveData.RList.t;
    set_root_widgets : ?step:React.step -> W.t M.Child_map.t -> unit;
  }

  let update t ~old_widgets ~parent_id =
    let rec make_or_update ~parent_id node =
      let id, new_item =
        match M.item node with
        | `Item (id, new_item) -> (Item_id id, Some new_item)
        | `Group s -> (Group_id (s, parent_id), None) in
      try
        let existing = Id_map.find id old_widgets in
        M.children node |> M.Child_map.map (make_or_update ~parent_id:id) |> existing.W.set_child_widgets;
        t.widgets := !(t.widgets) |> Id_map.add id existing;
        begin match existing.W.item, new_item with
        | `Item (_, set_item), Some new_item -> set_item new_item
        | `Group _, None -> ()
        | _ -> assert false end;
        existing
      with Not_found ->
        make_widget ~get_child:make_or_update ~widgets:t.widgets ~parent_id node
    in
    M.Child_map.map (make_or_update ~parent_id)

  let make nodes =
    let widgets = ref Id_map.empty in
    let rec get_child ~parent_id node =
      make_widget ~widgets ~get_child ~parent_id node in
    let root_widgets, set_root_widgets = make_widgets ~get_child ~parent_id:Root_id nodes in
    {
      widgets;
      root_widgets;
      set_root_widgets;
    }

  let update t ~on_remove nodes =
    (* print_endline "\n== update ==\n"; *)
    let old_widgets = !(t.widgets) in
    t.widgets := Id_map.empty;
    update t ~old_widgets ~parent_id:Root_id nodes |> t.set_root_widgets;

    old_widgets |> Id_map.iter (fun k old ->
      if not (Id_map.mem k !(t.widgets)) then (
        match old.W.item with
        | `Group _ -> ()
        | `Item (old_item, set_item) ->
            match on_remove (React.S.value old_item) with
            | None -> ()
            | Some update -> set_item update
      )
    )

  let widgets t = t.root_widgets
end
