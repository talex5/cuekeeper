(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_sigs

module Node = Ck_node
module M = Ck_node.M

module Make(Clock : Ck_clock.S)(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Ck_rev.Make(I)

  module TreeNode = struct
    module Id_map = Ck_id.M
    module Child_map = M
    module Sort_key = Node.SortKey

    module Item = struct
      include Node    (* We reuse Node.t, but ignore its children *)

      let equal a b =
        Node.uuid a = Node.uuid b &&
        Ck_disk_node.equal (Node.disk_node a) (Node.disk_node b)
      let show = name
      let id = Node.uuid
    end

    type t = {
      item : Node.generic;
      children : t M.t;
    }

    let item t = t.item
    let children t = t.children

    let leaf_of_node n = {
      item = n;
      children = M.empty;
    }

    let id t = Item.id t.item

    type move_data = int
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget

  type tree_view =
    [ `Process of Widget.t ReactiveData.RList.t
    | `Work of Widget.t ReactiveData.RList.t
    | `Sync of (float * string) list React.S.t
    | `Contact of unit
    | `Review of unit
    | `Schedule of unit ]

  type details = {
    details_item : Item.generic option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  type t = {
    mutable r : R.t;
    tree : tree_view React.S.t;
    set_tree : tree_view -> unit;
    mutable details : (details * (R.t -> unit)) Ck_id.M.t;
    mutable update_tree : R.t -> unit;
  }

  type 'a full_node = 'a Node.t

  let assume_changed _ _ = false

  let is_root = (=) Ck_id.root

  let set_current t r =
    t.r <- r;
    t.details |> Ck_id.M.iter (fun _id (_, set) -> set r);
    t.update_tree r

(*
  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan prefix x =
      let full_path = prefix ^ "/" ^ Node.name x in
      results := (full_path, x) :: !results;
      Node.child_nodes x |> M.iter (fun _k child ->
        match child with
        | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as x -> scan full_path x
        | _ -> ()
      ) in
    scan "" (root t |> React.S.value);
    List.rev !results
*)

  let add details t ~parent ~name ~description =
    R.add t.r details ~parent ~name ~description >|= fun (new_id, r_new) ->
    set_current t r_new;
    new_id

  let add_action = add (`Action {Ck_sigs.astate = `Next; astarred = false})
  let add_project = add (`Project {Ck_sigs.pstate = `Active; pstarred = false})
  let add_area = add `Area

  let delete t node =
    R.delete t.r node >|= set_current t

  let set_name t uuid name =
    let r = t.r in
    let node = R.get_exn r uuid in
    R.set_name r node name >|= set_current t

  let set_details t uuid new_details =
    let r = t.r in
    let node = R.get_exn r uuid in
    R.set_details r node new_details >|= set_current t

  let set_action_state t item state =
    R.set_action_state t.r item state >|= set_current t

  let set_project_state t item state =
    R.set_project_state t.r item state >|= set_current t

  let set_starred t item s =
    R.set_starred t.r item s >|= set_current t

  let make_full_tree r =
    let rec aux items =
      items |> M.map (fun item ->
        { TreeNode.item;
          children = aux (Node.child_nodes item) }
      ) in
    aux (Node.child_nodes (R.root r))

  let make_process_tree = make_full_tree

  let is_next_action _k node =
    match Node.ty node with
    | `Action a -> Node.action_state a = `Next
    | _ -> false

  let collect_next_actions r =
    let results = ref TreeNode.Child_map.empty in
    let rec scan node =
      match Node.ty node with
      | `Area parent | `Project parent ->
          let actions = Node.child_nodes parent |> M.filter is_next_action in
          if not (M.is_empty actions) then (
            let tree_node = { TreeNode.
              item = parent;
              children = actions |> M.map TreeNode.leaf_of_node
            } in
            results := !results |> M.add (Node.key parent) tree_node;
          );
          Node.child_nodes parent |> M.iter (fun _k v -> scan v)
      | `Action _ -> ()
    in
    scan (R.root r);
    !results

  let details t uuid =
    try fst (Ck_id.M.find uuid t.details)
    with Not_found ->
      let initial_node = R.get_exn t.r uuid in
      let child_nodes node = Node.child_nodes node |> M.map TreeNode.leaf_of_node in
      let children = WidgetTree.make (child_nodes initial_node) in
      let node, set_node = React.S.create (Some initial_node) in
      let update r =
        let node = R.get r uuid in
        set_node node;
        match node with
        | None -> WidgetTree.update children M.empty ~on_remove:(R.get r)
        | Some node -> WidgetTree.update children (child_nodes node) ~on_remove:(R.get r) in
      let details = {
        details_item = node;
        details_children = WidgetTree.widgets children;
        details_stop = (fun () ->
          t.details <- t.details |> Ck_id.M.remove uuid;
          ignore children
        );
      } in
      t.details <- t.details |> Ck_id.M.add uuid (details, update);
      details

  let initialise t =
    let add ~uuid ?parent ~name ~description details =
      let parent =
        match parent with
        | None -> Ck_id.root
        | Some p -> p in
      R.add t.r ~uuid:(Ck_id.of_string uuid) details ~parent ~name ~description >|= fun (uuid, r_new) ->
      set_current t r_new;
      uuid in
    (* Add some default entries for first-time use.
     * Use fixed UUIDs for unit-testing and in case we want to merge stores later. *)
    add
      ~uuid:"ad8c5bb1-f6b7-4a57-b090-d6ef2e3326c1"
      ~name:"Personal"
      ~description:"Add personal sub-areas here (Family, Car, Home, Exercise, etc)."
      `Area
    >>= fun personal ->

    add
      ~uuid:"1a7c8ea2-18ac-41cb-8f79-3566e49445f4"
      ~parent:personal
      ~name:"Start using CueKeeper"
      ~description:""
      (`Project {pstate = `Active; pstarred = false})
    >>= fun switch_to_ck ->

    add
      ~uuid:"6002ea71-6f1c-4ba9-8728-720f4b4c9845"
      ~parent:switch_to_ck
      ~name:"Read wikipedia page on GTD"
      ~description:"http://en.wikipedia.org/wiki/Getting_Things_Done"
      (`Action {astate = `Next; astarred = false})
    >>= fun _ ->

    add
      ~uuid:"1c6a6964-e6c8-499a-8841-8cb437e2930f"
      ~name:"Work"
      ~description:"Add work-related sub-areas here."
      `Area
    >>= fun _ ->
    return ()

  let rtree r fn =
    let rtree = WidgetTree.make (fn r) in
    let update_tree r =
      WidgetTree.update rtree (fn r) ~on_remove:(R.get r) in
    let widgets = WidgetTree.widgets rtree in
    (widgets, update_tree)

  let make_history r =
    let history, set_history = React.S.create (R.history r) in
    let update_tree r = set_history (R.history r) in
    history, update_tree

  let make_tree r = function
    | `Process -> let t, u = rtree r make_process_tree in `Process t, u
    | `Work -> let t, u = rtree r collect_next_actions in `Work t, u
    | `Review -> `Review (), ignore
    | `Contact -> `Contact (), ignore
    | `Schedule -> `Schedule (), ignore
    | `Sync -> let t, u = make_history r in `Sync t, u

  let set_mode t mode =
    let tree_view, update_tree = make_tree t.r mode in
    t.update_tree <- update_tree;
    t.set_tree tree_view

  let tree t = t.tree

  let make store =
    R.make store >>= fun r ->
    let rtree, update_tree = make_tree r `Work in
    let tree, set_tree = React.S.create ~eq:assume_changed rtree in

    let t = { r; tree; set_tree; update_tree; details = Ck_id.M.empty } in
    if M.is_empty (Node.child_nodes (R.root r)) then (
      initialise t >>= fun () -> return t
    ) else return t
end
