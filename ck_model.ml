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

    let with_child child parent =
      {parent with children = parent.children |> add child}

    let item t =
      match t.item with
      | `Item node -> `Item (Item.id node, node)
      | `Group _ as g -> g

    let children t = t.children

    let group ~pri label = {
      item = `Group (pri, label);
      children = Child_map.empty;
    }

    let leaf_of_node n = {
      item = `Item (n :> Item.generic);
      children = Child_map.empty;
    }
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)(G)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget
  open Item.Types

  type review_mode = [ `Waiting | `Future | `Areas | `Everything ]

  type tree_view =
    [ `Process of Widget.t ReactiveData.RList.t
    | `Work of Widget.t ReactiveData.RList.t
    | `Contact of Widget.t ReactiveData.RList.t
    | `Review of review_mode * Widget.t ReactiveData.RList.t
    | `Schedule of Widget.t ReactiveData.RList.t ]

  type details = {
    details_item : Item.generic option React.S.t;
    details_parent : [ area | project | action ] option React.S.t;
    details_context : context option option React.S.t;
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
    alert : bool React.S.t;
    mutable review_mode : review_mode;
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

  let add_action t ~state = add (Ck_disk_node.make_action ?context:None ~state) t
  let add_project t = add Ck_disk_node.make_project t
  let add_area t = add Ck_disk_node.make_area t

  let add_contact t ~name =
    let disk_node = Ck_disk_node.make_contact ~name ~description:"" ~ctime:(Clock.now ()) in
    Up.add_contact t.master ~base:t.r disk_node >|= fun id ->
    match R.get_contact t.r id with
    | None -> None
    | Some x -> Some (`Contact x)

  let add_context t ~name =
    let disk_node = Ck_disk_node.make_context ~name ~description:"" ~ctime:(Clock.now ()) in
    Up.add_context t.master ~base:t.r disk_node >|= fun id ->
    match R.get_context t.r id with
    | None -> None
    | Some x -> Some (`Context x)

  let set_context t item context =
    match R.get t.r (Node.uuid (`Action item)), R.get_context t.r (Node.uuid (`Context context)) with
    | Some (`Action action), Some context -> Up.set_context t.master action (Some context) >|= fun () -> `Ok ()
    | _, None -> `Error "Context no longer exists!" |> return
    | _, _ -> `Error "Action no longer exists!" |> return

  let add_child t parent name =
    match parent with
    | `Area _ as a -> add_project t ~parent:a ~name ~description:""
    | `Project _ as p -> add_action t ~state:`Next ~parent:p ~name ~description:""

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

  let or_existing parent item =
    try TreeNode.Child_map.find (TreeNode.sort_key item) parent
    with Not_found -> item

  let make_waiting_tree r =
    let waiting = TreeNode.group ~pri:(-1) "(unspecified reason)" in
    let results = ref TreeNode.Child_map.empty in
    let add ~group ~parent item =
      let group_item = or_existing !results group in
      let group_item =
        match parent with
        | None ->
            group_item |> TreeNode.with_child (TreeNode.leaf_of_node item)
        | Some p ->
            let parent_item =
              TreeNode.group ~pri:1 (Node.name p)
              |> or_existing group_item.TreeNode.children
              |> TreeNode.with_child (TreeNode.leaf_of_node item) in
            group_item |> TreeNode.with_child parent_item in
      results := !results |> TreeNode.add group_item in
    let rec scan ?parent _key = function
      | `Area _ | `Project _ as node ->
          R.child_nodes node |> M.iter (scan ~parent:node)
      | `Action action as node ->
          match Node.action_state action with
          | `Waiting -> add ~group:waiting ~parent node
          | `Waiting_for_contact c ->
              begin match R.get_contact r c with
              | Some contact -> add ~group:(TreeNode.leaf_of_node (`Contact contact)) ~parent node
              | None -> assert false end
          | _ -> () in
    R.roots r |> M.iter (scan ?parent:None);
    !results

  let make_review_tree ~mode r =
    match mode with
    | `Waiting -> make_waiting_tree r
    | _ -> make_full_tree r

  let make_contact_tree r =
    let contacts = R.contacts r in
    Ck_id.M.fold (fun _key item acc ->
      let value =
        { TreeNode.item = `Item (`Contact item);
          children = TreeNode.Child_map.empty } in
      acc |> TreeNode.add value
    ) contacts TreeNode.Child_map.empty

  let make_schedule_tree r =
    let day = ref None in
    let results = ref TreeNode.Child_map.empty in
    R.schedule r |> List.iter (fun action ->
      match Node.action_state action with
      | `Waiting_until time ->
          let date = fmt_date time in
          let group =
            match !day with
            | Some (prev_date, group) when prev_date = date -> group
            | _ -> TreeNode.group ~pri:0 date in
          let node = TreeNode.leaf_of_node (`Action action) in
          let group = {group with TreeNode.children = TreeNode.add node group.TreeNode.children} in
          day := Some (date, group);
          results := TreeNode.add group !results
      | _ -> assert false
    );
    !results

  let is_someday_project p =
    Node.project_state p = `SomedayMaybe

  let make_work_tree r =
    let next_actions = ref TreeNode.Child_map.empty in
    let add_next context parent item =
      let context_item =
        match context with
        | None -> TreeNode.group ~pri:(-1) "(no context)"
        | Some c -> TreeNode.leaf_of_node (`Context c) in
      let context_item = or_existing !next_actions context_item in
      let context_item =
        match parent with
        | None ->
            context_item |> TreeNode.with_child (TreeNode.leaf_of_node item)
        | Some p ->
            let group_item =
              TreeNode.group ~pri:1 (Node.name p)
              |> or_existing context_item.TreeNode.children
              |> TreeNode.with_child (TreeNode.leaf_of_node item) in
            context_item |> TreeNode.with_child group_item in
      next_actions := !next_actions |> TreeNode.add context_item in

    let done_items = ref TreeNode.Child_map.empty in
    let rec scan ?parent ~in_someday nodes =
      nodes |> M.iter (fun _k node ->
        match node with
        | `Project project when Node.project_state project = `Done ->
            let item = TreeNode.leaf_of_node node in
            done_items := !done_items |> TreeNode.add item
        | `Project parent ->
            let in_someday = in_someday || is_someday_project parent in
            R.child_nodes node |> scan ~parent:node ~in_someday
        | `Area _ ->
            R.child_nodes node |> scan ~parent:node ~in_someday
        | `Action action ->
            let add () =
              add_next (R.context action) parent node in
            match Node.action_state action with
            | `Next when not in_someday -> add ()
            | `Waiting_until _ when Node.is_due action -> add ()
            | `Done ->
                let item = TreeNode.leaf_of_node node in
                done_items := !done_items |> TreeNode.add item
            | _ -> ()
      ) in
    scan ?parent:None ~in_someday:false (R.roots r);
    TreeNode.Child_map.empty
    |> TreeNode.add {TreeNode.item = `Group (0, "Next actions"); children = !next_actions}
    |> TreeNode.add {TreeNode.item = `Group (0, "Recently completed"); children = !done_items}

  let opt_node_equal a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> R.Node.equal (a :> Node.generic) (b :> Node.generic)
    | _ -> false

  let opt_opt_node_equal a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> opt_node_equal a b
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
          | `Waiting | `Waiting_for_contact _ | `Waiting_until _ -> (3, "Waiting actions")
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
      details_context = React.S.const None;
      details_children = ReactiveData.RList.empty;
      details_stop = ignore
    }

  let add_waiting_info node tree =
    match node with
    | `Area _ | `Project _ -> tree
    | `Action action ->
        match Node.action_state action with
        | `Waiting_for_contact c ->
            begin match R.get_contact (Node.rev node) c with
            | Some contact ->
                let c_node = TreeNode.leaf_of_node (`Contact contact) in
                let children = TreeNode.Child_map.empty |> TreeNode.add c_node in
                let waiting_for = {TreeNode.item = `Group (0, "Waiting for"); children} in
                TreeNode.add waiting_for tree
            | None -> tree end
        | _ -> tree

  let on_remove r = function
    | `Area _ | `Project _ | `Action _ as node ->
        (R.get r (Node.uuid node) :> Node.generic option)
    | `Context _ as node ->
        begin match R.get_context r (Node.uuid node) with
        | None -> None
        | Some c -> Some (`Context c) end
    | `Contact _ as node ->
        match R.get_contact r (Node.uuid node) with
        | None -> None
        | Some c -> Some (`Contact c)

  let make_details t ~initial_node ~child_nodes ?get_parent ?get_context ~get ~wrap ~ok () =
    let uuid = Node.uuid initial_node in
    match get t.r uuid with
    | None -> deleted_details
    | Some initial_node ->
        let parent, update_parent =
          match get_parent with
          | None -> React.S.const None, (fun _ _  -> ())
          | Some get_parent ->
              let parent, set_parent = React.S.create ~eq:opt_node_equal (get_parent t.r initial_node) in
              let update r = function
                | None -> set_parent None
                | Some node -> set_parent (get_parent r node) in
              parent, update in
        let context, update_context =
          match get_context with
          | None -> React.S.const None, ignore
          | Some get_context ->
              let context, set_context = React.S.create ~eq:opt_opt_node_equal (get_context (Some initial_node)) in
              let update c = set_context (get_context c) in
              context, update in
        let node, set_node = React.S.create ~eq:opt_node_equal (Some (wrap initial_node)) in
        let children = WidgetTree.make (child_nodes initial_node) in
        let update r =
          let node = get r uuid in
          update_parent r node;
          update_context node;
          match node with
          | None ->
              set_node None;
              WidgetTree.update children TreeNode.Child_map.empty ~on_remove:(on_remove r)
          | Some node ->
              set_node (Some (wrap node));
              WidgetTree.update children (child_nodes node) ~on_remove:(on_remove r) in
        ok ~node ~parent ~context ~children:(WidgetTree.widgets children) ~update

  let details t initial_node =
    let initial_node = (initial_node :> Node.generic) in
    let uuid = Node.uuid initial_node in
    try fst (Ck_id.M.find uuid t.details)
    with Not_found ->
      let ok ~node ~parent ~context ~children ~update =
        let details = {
          details_item = node;
          details_parent = parent;
          details_context = context;
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
      | `Context _ ->
          let child_nodes c =
            R.actions_of_context c
            |> List.fold_left (fun acc action ->
              acc |> TreeNode.add (TreeNode.leaf_of_node (`Action action))
            ) TreeNode.Child_map.empty in
          let get r id = R.get_context r id in
          let wrap x = `Context x in
          make_details t ~initial_node ~child_nodes ~get ~wrap ~ok ()
      | `Contact _ ->
          let child_nodes c =
            R.actions_of_contact c
            |> List.fold_left (fun acc action ->
              acc |> TreeNode.add (TreeNode.leaf_of_node (`Action action))
            ) TreeNode.Child_map.empty in
          let get r id = R.get_contact r id in
          let wrap x = `Contact x in
          make_details t ~initial_node ~child_nodes ~get ~wrap ~ok ()
      | `Area _ | `Project _ | `Action _ as initial_node ->
          let child_nodes node =
            R.child_nodes node
            |> group_by_type ~parent:node
            |> add_waiting_info node in
          let wrap x = (x :> Item.generic) in
          let get = R.get in
          let get_context = function
            | None -> None
            | Some (`Area _ | `Project _) -> None
            | Some (`Action action) ->
                match R.context action with
                | None -> Some None
                | Some c -> Some (Some (`Context c)) in
          make_details t ~initial_node ~child_nodes ~get ~wrap ~get_parent:R.parent ~get_context ~ok ()

  type candidate = string * (unit -> unit Lwt.t)
  let candidate_label = fst
  let choose_candidate (_, set) = set ()

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
    (* Item may be from an older revision, but we want the current parents as options. *)
    match R.get t.r (Item.uuid item) with
    | None -> []    (* Item has been deleted *)
    | Some item ->
    match item with
    | `Project _ | `Action _ as node -> candidate_parents_for_pa t node
    | `Area _ as node -> candidate_parents_for_a t node

  let candidate_contacts_for t item =
    (* Item may be from an older revision, but we want the current contacts as options. *)
    match R.get t.r (Item.uuid item) with
    | None -> []    (* Item has been deleted *)
    | Some (`Area _ | `Project _) -> []   (* Action has been converted to something else *)
    | Some (`Action action) ->
        let contacts =
          R.contacts t.r |> Ck_id.M.bindings |> List.map (fun (_id, contact) ->
            ("for " ^ Node.name (`Contact contact), fun () -> Up.set_action_state t.master action (`Waiting_for_contact contact))
          )
          |> List.sort (fun a b -> compare (fst a) (fst b)) in
        let waiting = ("Waiting", fun () -> Up.set_action_state t.master action `Waiting) in
        waiting :: contacts

  let candidate_contexts_for t item =
    (* Item may be from an older revision, but we want the current contexts as options. *)
    match R.get t.r (Item.uuid item) with
    | None -> []    (* Item has been deleted *)
    | Some (`Area _ | `Project _) -> []   (* Action has been converted to something else *)
    | Some (`Action action) ->
        let contexts =
          R.contexts t.r |> Ck_id.M.bindings |> List.map (fun (_id, context) ->
            (Node.name (`Context context), fun () -> Up.set_context t.master action (Some context))
          )
          |> List.sort (fun a b -> compare (fst a) (fst b)) in
        let none = ("(no context)", fun () -> Up.set_context t.master action None) in
        none :: contexts

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

    Up.add_context t.master ~base:t.r ~uuid:(Ck_id.of_string "c6776794-d53e-460a-ada7-7e3b98c2f126")
      (Ck_disk_node.make_context
        ~name:"Reading"
        ~description:"Reading books, web-sites, etc."
        ~ctime:(Clock.now ()))
    >>= fun reading ->

    add
      ~uuid:"6002ea71-6f1c-4ba9-8728-720f4b4c9845"
      ~parent:switch_to_ck
      (Ck_disk_node.make_action
        ~state:`Next
        ~context:reading
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
    | `Review mode -> let t, u = rtree r (make_review_tree ~mode) in `Review (mode, t), u
    | `Contact -> let t, u = rtree r make_contact_tree in `Contact t, u
    | `Schedule -> let t, u = rtree r make_schedule_tree in `Schedule t, u

  let set_mode t mode =
    let mode =
      match mode with
      | `Process | `Work | `Contact | `Schedule as m -> m
      | `Review -> `Review t.review_mode in
    let tree_view, update_tree = make_tree t.r mode in
    t.update_tree <- update_tree;
    t.set_tree tree_view

  let set_review_mode t mode =
    t.review_mode <- mode;
    set_mode t `Review

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
    let alert, set_alert = React.S.create (R.alert r) in
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
      alert;
      fixed_head; set_fixed_head;
      details = Ck_id.M.empty;
      review_mode = `Waiting;
      keep_me = []
    } in
    Lwt.wakeup set_on_update (fun r ->
      set_alert (R.alert r);
      t.r <- r;
      t.details |> Ck_id.M.iter (fun _id (_, set) -> set r);
      t.update_tree r;
      if not (Up.fixed_head t.master) then set_fixed_head None;
      get_log master >|= set_log
    );
    if M.is_empty (R.roots r) then (
      initialise t >>= fun () -> return t
    ) else return t

  let alert t = t.alert
end
