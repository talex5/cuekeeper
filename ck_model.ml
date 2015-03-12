(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_sigs
open Ck_utils

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (G : GUI_DATA) = struct
  module R = Ck_rev.Make(Git)
  module Node = R.Node
  module Up = Ck_update.Make(Git)(Clock)(R)
  module Slow_history = Slow_set.Make(Clock)(Git_storage_s.Log_entry)(Git_storage_s.Log_entry_map)
  module Slow_log_entry = struct
    type t = Git_storage_s.Log_entry.t Slow_set.item
    let equal a b = Git_storage_s.Log_entry.equal (Slow_set.data a) (Slow_set.data b)
  end
  module Delta_history = Delta_RList.Make(Git_storage_s.Log_entry)(Slow_log_entry)(Git_storage_s.Log_entry_map)

  type gui_data = G.t

  module TreeNode = struct
    type group_id = int * string    (* int is the sort order *)
    let group_label (_, s) = s

    module Id_map = Ck_id.M
    module Sort_key = struct
      type t =
        | Item of Sort_key.t
        | Group of group_id
      module Id = struct
        type t =
          | Item of Ck_id.t
          | Group of group_id
        let compare = compare
      end
      let compare a b =
        match a, b with
        | Item _, Group _ -> 1
        | Group _, Item _ -> -1
        | Item a, Item b -> Sort_key.compare a b
        | Group a, Group b -> compare a b
      let show = function
        | Item a -> Sort_key.show a
        | Group (_, s) -> s
      let id = function
        | Item a -> Id.Item (Sort_key.id a)
        | Group s -> Id.Group s
    end
    module Child_map = Map.Make(Sort_key)

    module Item = struct
      include Node
      let show = name
      let id = Node.uuid
    end

    type t = {
      item : [`Item of Node.generic | `Group of group_id];
      children : t Child_map.t;
    }

    let sort_key t =
      match t.item with
      | `Item i -> Sort_key.Item (R.Node.key i)
      | `Group g -> Sort_key.Group g

    let add item map =
      map |> Child_map.add (sort_key item) item

    let item t =
      match t.item with
      | `Item node -> `Item (Item.id node, node)
      | `Group _ as g -> g

    let children t = t.children

    let leaf_of_node n = {
      item = `Item (n :> Item.generic);
      children = Child_map.empty;
    }
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)(G)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget
  open Item.Types

  type tree_view =
    [ `Process of Widget.t ReactiveData.RList.t
    | `Work of Widget.t ReactiveData.RList.t
    | `Contact of Widget.t ReactiveData.RList.t
    | `Review of Widget.t ReactiveData.RList.t
    | `Schedule of unit ]

  type details = {
    details_item : Item.generic option React.S.t;
    details_parent : [ area | project | action ] option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  type t = {
    repo : Git.Repository.t;
    master : Up.t;
    mutable r : R.t;
    tree : tree_view React.S.t;
    set_tree : tree_view -> unit;
    log : Git_storage_s.Log_entry.t Slow_set.item ReactiveData.RList.t;
    fixed_head : Git_storage_s.Log_entry.t option React.S.t;
    set_fixed_head : Git_storage_s.Log_entry.t option -> unit;
    mutable details : (details * (R.t -> unit)) Ck_id.M.t;
    mutable update_tree : R.t -> unit;
    mutable keep_me : unit React.S.t list;
  }

  let assume_changed _ _ = false

  let add maker t ?parent ~name ~description =
    let parent =
      match parent with
      | None -> `Toplevel t.r
      | Some p -> `Node p in
    let disk_node = maker ~name ~description in
    Up.add t.master ~parent disk_node >|= fun id ->
    R.get t.r id

  let add_action t = add Ck_disk_node.make_action t
  let add_project t = add Ck_disk_node.make_project t
  let add_area t = add Ck_disk_node.make_area t

  let add_contact t ~name =
    let disk_node = Ck_disk_node.make_contact ~name ~description:"" ~ctime:(Unix.gettimeofday ()) in
    Up.add_contact t.master ~base:t.r disk_node >|= fun id ->
    match R.get_contact t.r id with
    | None -> None
    | Some x -> Some (`Contact x)

  let add_child t parent name =
    match parent with
    | `Area _ as a -> add_project t ~parent:a ~name ~description:""
    | `Project _ as p -> add_action t ~parent:p ~name ~description:""

  let delete t node =
    Up.delete t.master node

  let set_name t item name =
    Up.set_name t.master item name

  let set_description t item v =
    Up.set_description t.master item v

  let set_action_state t item state =
    Up.set_action_state t.master item state

  let set_project_state t item state =
    Up.set_project_state t.master item state

  let set_starred t item s =
    Up.set_starred t.master item s

  let convert_to_project t item =
    Up.convert_to_project t.master item

  let convert_to_area t item =
    Up.convert_to_area t.master item

  let convert_to_action t item =
    Up.convert_to_action t.master item

  let make_full_tree r =
    let rec aux items =
      M.fold (fun key item acc ->
        let value =
          { TreeNode.item = `Item (item :> Node.generic);
            children = aux (R.child_nodes item) } in
        acc |> TreeNode.Child_map.add (TreeNode.Sort_key.Item key) value
      ) items TreeNode.Child_map.empty in
    aux (R.roots r)

  let make_process_tree r =
    let rec aux items =
      M.fold (fun key item acc ->
        match item with
        | `Action _ -> acc
        | `Area _ | `Project _ ->
            let value =
              { TreeNode.item = `Item (item :> Item.generic);
                children = aux (R.child_nodes item) } in
            acc |> TreeNode.Child_map.add (TreeNode.Sort_key.Item key) value
      ) items TreeNode.Child_map.empty in
    aux (R.roots r)

  let make_review_tree = make_full_tree

  let make_contact_tree r =
    let contacts = R.contacts r in
    Ck_id.M.fold (fun _key item acc ->
      let value =
        { TreeNode.item = `Item (`Contact item);
          children = TreeNode.Child_map.empty } in
      acc |> TreeNode.add value
    ) contacts TreeNode.Child_map.empty

  let is_someday_project p =
    Node.project_state p = `SomedayMaybe

  let make_work_tree r =
    let next_actions = ref TreeNode.Child_map.empty in
    let done_items = ref TreeNode.Child_map.empty in
    let rec scan ~in_someday nodes =
      let child_actions = ref TreeNode.Child_map.empty in
      nodes |> M.iter (fun _k node ->
        match node with
        | `Project project when Node.project_state project = `Done ->
            let item = TreeNode.leaf_of_node node in
            done_items := !done_items |> TreeNode.add item
        | `Project parent ->
            let in_someday = in_someday || is_someday_project parent in
            scan_container ~in_someday node
        | `Area _ -> scan_container ~in_someday node
        | `Action action ->
            match Node.action_state action with
            | `Next when not in_someday ->
                let item = TreeNode.leaf_of_node node in
                child_actions := !child_actions |> TreeNode.add item
            | `Done ->
                let item = TreeNode.leaf_of_node node in
                done_items := !done_items |> TreeNode.add item
            | _ -> ()
      );
      !child_actions
    and scan_container ~in_someday parent =
      let actions = R.child_nodes parent |> scan ~in_someday in
      if not (TreeNode.Child_map.is_empty actions) then (
        let tree_node = { TreeNode.
          item = `Item (parent :> Item.generic);
          children = actions;
        } in
        next_actions := !next_actions |> TreeNode.add tree_node;
      )
    in
    let root_actions = scan ~in_someday:false (R.roots r) in
    if not (TreeNode.Child_map.is_empty root_actions) then (
      let no_project = { TreeNode.item = `Group (0, "(no project)"); children = root_actions } in
      next_actions := !next_actions |> TreeNode.add no_project;
    );
    TreeNode.Child_map.empty
    |> TreeNode.add {TreeNode.item = `Group (0, "Next actions"); children = !next_actions}
    |> TreeNode.add {TreeNode.item = `Group (0, "Recently completed"); children = !done_items}

  let opt_node_equal a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> R.Node.equal (a :> Node.generic) (b :> Node.generic)
    | _ -> false

  let group_by_type ~parent child_nodes =
    let tree_nodes = ref TreeNode.Child_map.empty in
    let group_for node =
      match parent, node with
      | _, `Area _ -> (0, "Sub-areas")
      | `Area _, `Project _ -> (1, "Projects")
      | _, `Project _ -> (1, "Sub-projects")
      | _, (`Action a) ->
          match Node.action_state a with
          | `Next -> (2, "Next actions")
          | `Waiting -> (3, "Waiting actions")
          | `Future -> (4, "Future actions")
          | `Done -> (5, "Completed actions") in
    let add node =
      let group_name = group_for node in
      let key = TreeNode.Sort_key.Group group_name in
      let parent =
        try TreeNode.Child_map.find key !tree_nodes
        with Not_found ->
          let p = {TreeNode.item = `Group group_name; children = TreeNode.Child_map.empty} in
          tree_nodes := !tree_nodes |> TreeNode.add p;
          p in
      let children = parent.TreeNode.children |> TreeNode.add (TreeNode.leaf_of_node node) in
      tree_nodes := !tree_nodes |> TreeNode.add {parent with TreeNode.children} in
    child_nodes |> M.iter (fun _k v -> add v);
    !tree_nodes

  let deleted_details =
    {
      details_item = React.S.const None;
      details_parent = React.S.const None;
      details_children = ReactiveData.RList.empty;
      details_stop = ignore
    }

  let details t initial_node =
    let initial_node = (initial_node :> Node.generic) in
    let uuid = Node.uuid initial_node in
    try fst (Ck_id.M.find uuid t.details)
    with Not_found ->
      let ok ~node ~parent ~children ~update =
        let details = {
          details_item = node;
          details_parent = parent;
          details_children = children;
          details_stop = (fun () ->
            t.details <- t.details |> Ck_id.M.remove uuid;
            ignore children
          );
        } in
        t.details <- t.details |> Ck_id.M.add uuid (details, update);
        details in
      (* Note: initial_node may already be out-of-date *)
      match initial_node with
      | `Contact _ ->
          begin match R.get_contact t.r (Node.uuid initial_node) with
          | None -> deleted_details
          | Some initial_node ->
              let node, set_node = React.S.create ~eq:opt_node_equal (Some (`Contact initial_node)) in
              let update r =
                let node =
                  match R.get_contact r uuid with
                  | None -> None
                  | Some node -> Some (`Contact node) in
                set_node node in
              ok ~node ~parent:(React.S.const None) ~children:ReactiveData.RList.empty ~update
          end
      | `Area _ | `Project _ | `Action _ as initial_node ->
          match R.get t.r (Node.uuid initial_node) with
          | None -> deleted_details
          | Some initial_node ->
              let initial_parent = R.parent t.r initial_node in
              let child_nodes node = R.child_nodes node |> group_by_type ~parent:node in
              let children = WidgetTree.make (child_nodes initial_node) in
              let parent, set_parent = React.S.create ~eq:opt_node_equal initial_parent in
              let node, set_node = React.S.create ~eq:opt_node_equal (Some (initial_node :> Item.generic)) in
              let update r =
                let on_remove node = (R.get r (Node.uuid node) :> Node.generic option) in
                let node = R.get r uuid in
                set_node (node :> Item.generic option);
                match node with
                | None ->
                    set_parent None;
                    WidgetTree.update children TreeNode.Child_map.empty ~on_remove
                | Some node ->
                    set_parent (R.parent r node);
                    WidgetTree.update children (child_nodes node) ~on_remove in
              ok ~node ~parent ~children:(WidgetTree.widgets children) ~update

  type candidate_parent = string * (unit -> unit Lwt.t)
  let candidate_label = fst
  let set_parent (_, set) = set ()

  let candidate_parents_for_pa t item =
    let item_uuid = Node.uuid item in
    let results = ref ["(no parent)", fun () -> Up.remove_parent t.master item] in
    let rec scan ~indent nodes =
      nodes |> M.iter (fun key node ->
        if Sort_key.id key <> item_uuid then (
          match node with
          | `Area _ | `Project _ as node ->
              results := (indent ^ Node.name node, fun () -> Up.set_pa_parent t.master item node) :: !results;
              R.child_nodes node |> scan ~indent:(indent ^ "» ")
          | `Action _ -> ()
        )
      ) in
    R.roots t.r |> scan ~indent:"";
    List.rev !results

  let candidate_parents_for_a t item =
    let item_uuid = Node.uuid item in
    let results = ref ["(no parent)", fun () -> Up.remove_parent t.master item] in
    let rec scan ~indent nodes =
      nodes |> M.iter (fun key node ->
        if Sort_key.id key <> item_uuid then (
          match node with
          | `Area _ as node ->
              results := (indent ^ Node.name node, fun () -> Up.set_a_parent t.master item node) :: !results;
              R.child_nodes node |> scan ~indent:(indent ^ "» ")
          | `Project _ | `Action _ -> ()
        )
      ) in
    R.roots t.r |> scan ~indent:"";
    List.rev !results

  let candidate_parents_for t item =
    (* Item may be from an older revision, but the want the current parents as options. *)
    match R.get t.r (Item.uuid item) with
    | None -> []    (* Item has been deleted *)
    | Some item ->
    match item with
    | `Project _ | `Action _ as node -> candidate_parents_for_pa t node
    | `Area _ as node -> candidate_parents_for_a t node

  let initialise t =
    let add ~uuid ?parent disk_node =
      let parent =
        match parent with
        | None -> `Toplevel t.r
        | Some (`Area _ | `Project _ as p) -> `Node p
        | Some _ -> assert false in
      Up.add t.master ~uuid:(Ck_id.of_string uuid) ~parent disk_node >>= fun uuid ->
      match R.get t.r uuid with
      | None -> failwith "Created node does not exist!"
      | Some node -> return node
      in
    (* Add some default entries for first-time use.
     * Use fixed UUIDs for unit-testing and in case we want to merge stores later. *)
    add
      ~uuid:"ad8c5bb1-f6b7-4a57-b090-d6ef2e3326c1"
      (Ck_disk_node.make_area
        ~name:"Personal"
        ~description:"Add personal sub-areas here (Family, Car, Home, Exercise, etc).")
    >>= fun personal ->

    add
      ~uuid:"1a7c8ea2-18ac-41cb-8f79-3566e49445f4"
      ~parent:personal
      (Ck_disk_node.make_project
        ~name:"Start using CueKeeper"
        ~description:"")
    >>= fun switch_to_ck ->

    add
      ~uuid:"6002ea71-6f1c-4ba9-8728-720f4b4c9845"
      ~parent:switch_to_ck
      (Ck_disk_node.make_action
        ~name:"Read wikipedia page on GTD"
        ~description:"http://en.wikipedia.org/wiki/Getting_Things_Done")
    >>= fun _ ->

    add
      ~uuid:"1c6a6964-e6c8-499a-8841-8cb437e2930f"
      (Ck_disk_node.make_area
        ~name:"Work"
        ~description:"Add work-related sub-areas here.")
    >>= fun _ ->
    return ()

  let rtree r fn =
    let rtree = WidgetTree.make (fn r) in
    let update_tree r =
      let on_remove node = (R.get r (Node.uuid node) :> Node.generic option) in
      WidgetTree.update rtree (fn r) ~on_remove in
    let widgets = WidgetTree.widgets rtree in
    (widgets, update_tree)

  let get_log master =
    Up.branch_head master
    |> Git.Commit.history ~depth:10

  let make_tree r = function
    | `Process -> let t, u = rtree r make_process_tree in `Process t, u
    | `Work -> let t, u = rtree r make_work_tree in `Work t, u
    | `Review -> let t, u = rtree r make_review_tree in `Review t, u
    | `Contact -> let t, u = rtree r make_contact_tree in `Contact t, u
    | `Schedule -> `Schedule (), ignore

  let set_mode t mode =
    let tree_view, update_tree = make_tree t.r mode in
    t.update_tree <- update_tree;
    t.set_tree tree_view

  let tree t = t.tree
  let log t = t.log
  let fix_head t entry =
    t.set_fixed_head entry;
    match entry with
    | None -> Up.fix_head t.master None
    | Some entry ->
        Git.Repository.commit t.repo entry.Git_storage_s.Log_entry.id
        >>= Up.fix_head t.master

  let fixed_head t = t.fixed_head

  let init_repo staging =
    Git.Staging.update staging ["ck-version"] "0.1"

  let make repo =
    let on_update, set_on_update = Lwt.wait () in
    Git.Repository.branch ~if_new:init_repo repo "master" >>= Up.make ~on_update >>= fun master ->
    let r = Up.head master in
    get_log master >>= fun initial_log ->
    let log, set_log = React.S.create initial_log in
    let fixed_head, set_fixed_head = React.S.create None in
    let log =
      Slow_history.make ~delay:1.0 ~eq:Git_storage_s.Log_entry.equal log
      |> Delta_history.make in
    let rtree, update_tree = make_tree r `Work in
    let tree, set_tree = React.S.create ~eq:assume_changed rtree in
    let t = {
      repo; master; r;
      tree; set_tree; update_tree;
      log;
      fixed_head; set_fixed_head;
      details = Ck_id.M.empty;
      keep_me = []
    } in
    Lwt.wakeup set_on_update (fun r ->
      t.r <- r;
      t.details |> Ck_id.M.iter (fun _id (_, set) -> set r);
      t.update_tree r;
      if not (Up.fixed_head t.master) then set_fixed_head None;
      get_log master >|= set_log
    );
    if M.is_empty (R.roots r) then (
      initialise t >>= fun () -> return t
    ) else return t
end
