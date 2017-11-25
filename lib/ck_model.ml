(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_sigs
open Ck_utils

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (G : GUI_DATA)
           (RPC : RPC) = struct
  module R = Ck_rev.Make(Git)
  module Node = R.Node
  module Up = Ck_update.Make(Git)(Clock)(R)
  module Slow_history = Slow_set.Make(Clock)(Git_storage_s.Log_entry)(Git_storage_s.Log_entry_map)
  module Slow_log_entry = struct
    type t = Git_storage_s.Log_entry.t Slow_set.item
    let equal a b = Git_storage_s.Log_entry.equal (Slow_set.data a) (Slow_set.data b)
  end
  module Delta_history = Delta_RList.Make(Git_storage_s.Log_entry)(Slow_log_entry)(Git_storage_s.Log_entry_map)
  module Client = Ck_client.Make(Clock)(Git)(RPC)

  type gui_data = G.t

  module TreeNode = struct
    type group = int * string    (* int is the sort order *)
    let group_label (_, s) = s

    module Sort_key = struct
      type t =
        | Item of Sort_key.t
        | ItemGroup of Sort_key.t
        | Group of group
      module Id = struct
        type t =
          | Item of Ck_id.t
          | Group of group
        let compare = compare
      end
      let compare a b =
        match a, b with
        | Item a, Item b -> Sort_key.compare a b
        | Item _, _ -> -1
        | _, Item _ -> 1
        (* Items come before groups for non-indented lists *)
        | ItemGroup a, ItemGroup b -> Sort_key.compare a b
        | ItemGroup _, Group _ -> 1
        | Group _, ItemGroup _ -> -1
        | Group a, Group b -> compare a b
      let show = function
        | Item a | ItemGroup a -> Sort_key.show a
        | Group (_, s) -> s
      let id = function
        | Item a | ItemGroup a -> Id.Item (Sort_key.id a)
        | Group s -> Id.Group s
    end
    module Child_map = Map.Make(Sort_key)

    module Item = struct
      include Node
      let show = name
      let id = Node.uuid
      let contact_node = R.contact_for
    end

    open Item.Types
    type adder =
      | Add_action of [area | project] option * context option * contact option * action_state
      | Add_project of [area | project] option * project_state
      | Add_area of area option
      | Add_contact
      | Add_context

    type t = {
      item :
        [ `UniqueItem of Item.generic   (* ID is unique in tree *)
        | `GroupItem of Item.generic    (* ID is unique within parent *)
        | `Group of group ];            (* Label is unique within parent *)
      children : t Child_map.t;
      adder : adder option;
    }

    let sort_key t =
      match t.item with
      | `UniqueItem i -> Sort_key.Item (R.Node.key i)
      | `GroupItem i -> Sort_key.ItemGroup (R.Node.key i)
      | `Group g -> Sort_key.Group g

    let add item map =
      map |> Child_map.add (sort_key item) item

    let or_existing parent item =
      try Child_map.find (sort_key item) parent
      with Not_found -> item

    (* Add [/group*/child] to [top]
     * If any group (with the same key) already exists, add to that instead.
     * Since groups are only used as templates if the group is missing, they
     * must not contain children. *)
    let add_grouped ~top ~groups child =
      let rec aux top child = function
        | [] -> top |> add child
        | None :: gs -> aux top child gs
        | Some g :: gs ->
            assert (Child_map.is_empty g.children);
            let g = g |> or_existing top in
            let g = {g with children = aux g.children child gs} in
            top |> add g in
      top := aux !top child groups

    let item t =
      match t.item with
      | `UniqueItem node -> `UniqueItem (Item.id node, node)
      | `GroupItem node -> `GroupItem (Item.id node, node)
      | `Group _ as g -> g

    let children t = t.children
    let adder t = t.adder

    let group ~pri ?(children=Child_map.empty) ?adder label = {
      item = `Group (pri, label);
      children;
      adder;
    }

    let unique_of_node ?(children=Child_map.empty) ?adder n = {
      item = `UniqueItem (n :> Item.generic);
      children;
      adder;
    }

    let group_of_node ?(children=Child_map.empty) ?adder n = {
      item = `GroupItem (n :> Item.generic);
      children;
      adder;
    }

    let of_id_map_f fn m =
      Ck_id.M.fold (fun _key item acc ->
        match fn item with
        | None -> acc
        | Some item -> acc |> add item
      ) m Child_map.empty

    let of_list_f fn =
      List.fold_left (fun acc item ->
        match fn item with
        | None -> acc
        | Some item -> acc |> add item
      ) Child_map.empty

    let of_list fn = of_list_f (fun x -> Some (fn x))
    let of_id_map fn = of_id_map_f (fun x -> Some (fn x))
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)(G)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget
  open Item.Types

  type review_mode = [ `Done | `Waiting | `Future | `Areas | `Everything ]

  type filter = (area * bool) list

  type tree_view =
    [ `Process of Widget.t ReactiveData.RList.t
    | `Work of filter React.S.t * Widget.t ReactiveData.RList.t
    | `Contact of Widget.t ReactiveData.RList.t
    | `Review of review_mode * Widget.t ReactiveData.RList.t
    | `Schedule of Widget.t ReactiveData.RList.t ]

  type details = {
    details_item : Item.generic option React.S.t;
    details_parent : [ area | project | action ] option React.S.t;
    details_context : context option option React.S.t;
    details_contact : contact option React.S.t option;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  type t = {
    repo : Git.Repository.t;
    master : Up.t;
    mutable r : R.t;
    tree : tree_view React.S.t;
    set_tree : tree_view -> unit;
    mutable update_log : (?step:React.step -> Git_storage_s.Log_entry.t Git_storage_s.Log_entry_map.t -> unit) option;
    fixed_head : Git_storage_s.Log_entry.t option React.S.t;
    set_fixed_head : Git_storage_s.Log_entry.t option -> unit;
    mutable details : (details * (R.t -> unit)) Ck_id.M.t;
    mutable update_tree : R.t -> unit;
    mutable keep_me : unit React.S.t list;
    alert : bool React.S.t;
    mutable review_mode : review_mode;
    hidden_areas : Ck_id.S.t ref;   (* Filter in Work tab *)
    client : Client.t option;
    server_head : Irmin.Hash.SHA1.t option React.S.t;
  }

  module X : sig
    val freshen : t -> ([< Node.generic] as 'a) -> 'a
  end = struct
    let freshen t node = Obj.magic @@
      let uuid = Node.uuid node in
      match node with
      | `Area _ | `Project _ | `Action _ as node ->
          begin match R.get t.r uuid with
          | Some current when Node.equal current node -> (current :> Node.generic)
          | _ -> node end
      | `Contact _ as node ->
          begin match R.get_contact t.r uuid with
          | Some current when Node.equal current node -> (current :> Node.generic)
          | _ -> node end
      | `Context _ as node ->
          begin match R.get_context t.r uuid with
          | Some current when Node.equal current node -> (current :> Node.generic)
          | _ -> node end
  end
  let freshen = X.freshen

  let assume_changed _ _ = false

  let add maker t ?parent ~name ?(description="") () =
    let parent =
      match parent with
      | None -> `Toplevel t.r
      | Some p -> `Node (freshen t p) in
    let disk_node = maker ~name ~description in
    Up.add t.master ~parent disk_node >|= fun id ->
    R.get t.r id

  let add_action t ~state ?context ?contact =
    let context = context >|?= Node.uuid in
    let contact = contact >|?= Node.uuid in
    add (Ck_disk_node.make_action ?context ?contact ~state) t
  let add_project t ?(state=`Active) = add (Ck_disk_node.make_project ~state ?contact:None) t
  let add_area t = add (Ck_disk_node.make_area ?contact:None) t

  let add_contact t ~name () =
    let disk_node = Ck_disk_node.make_contact ~name ~description:"" ~ctime:(Clock.now ()) () in
    Up.add_contact t.master ~base:t.r disk_node >|= fun id ->
    R.get_contact t.r id

  let add_context t ~name () =
    let disk_node = Ck_disk_node.make_context ~name ~description:"" ~ctime:(Clock.now ()) () in
    Up.add_context t.master ~base:t.r disk_node >|= fun id ->
    R.get_context t.r id

  let set_context t item context =
    match R.get t.r (Node.uuid item), R.get_context t.r (Node.uuid context) with
    | Some (`Action _ as action), Some context -> Up.set_context t.master action (Some context) >|= fun () -> `Ok ()
    | _, None -> `Error "Context no longer exists!" |> return
    | _, _ -> `Error "Action no longer exists!" |> return

  let set_contact t item contact =
    match contact with
    | None -> Up.set_contact t.master item None >|= fun () -> `Ok ()
    | Some contact ->
        match R.get t.r (Node.uuid item), R.get_contact t.r (Node.uuid contact) with
        | Some node, Some contact -> Up.set_contact t.master node (Some contact) >|= fun () -> `Ok ()
        | _, None -> `Error "Contact no longer exists!" |> return
        | _, _ -> `Error "Item no longer exists!" |> return

  let add_child t parent name =
    match parent with
    | `Area _ as a -> add_project t ~parent:a ~name ~description:"" ()
    | `Project _ as p -> add_action t ~state:`Next ~parent:p ~name ~description:"" ()
    | `Context _ as context -> add_action t ~state:`Next ~context ~name ~description:"" ()

  let apply_adder t adder name =
    let ok x = (x :> Item.generic option) in
    match adder with
    | TreeNode.Add_action (parent, context, contact, state) -> add_action t ~state ?parent ?context ?contact ~name () >|= ok
    | TreeNode.Add_project (parent, state) -> add_project t ~state ?parent ~name () >|= ok
    | TreeNode.Add_area parent -> add_area t ?parent ~name () >|= ok
    | TreeNode.Add_contact -> add_contact t ~name () >|= ok
    | TreeNode.Add_context -> add_context t ~name () >|= ok

  let clear_conflicts t node =
    Up.clear_conflicts t.master (freshen t node)

  let delete t node =
    Up.delete t.master [freshen t node]

  let delete_done t =
    let to_delete = ref [] in
    let rec aux items =
      M.fold (fun _uuid item acc ->
        match item with
        | `Area _ ->
            let _children : bool = aux (R.child_nodes item) in true
        | `Project _ as p ->
            let children = aux (R.child_nodes item) in
            if R.Node.project_state p = `Done && not children then (
              to_delete := item :: !to_delete;
              acc
            ) else true
        | `Action _ as a ->
            if R.Node.action_state a = `Done then (
              to_delete := item :: !to_delete;
              acc
            ) else true
      ) items false in
    let _children : bool = aux (R.roots t.r) in
    Up.delete t.master ~msg:"Delete done items" !to_delete >|= function
    | `Error msg -> failwith msg
    | `Ok () -> ()

  let set_name t item name =
    Up.set_name t.master (freshen t item) name

  let set_description t item v =
    Up.set_description t.master (freshen t item) v

  let set_action_state t item state =
    Up.set_action_state t.master (freshen t item) state

  let set_repeat t item repeat =
    Up.set_repeat t.master (freshen t item) repeat

  let set_project_state t item state =
    Up.set_project_state t.master (freshen t item) state

  let set_starred t item s =
    Up.set_starred t.master (freshen t item) s

  let convert_to_project t item =
    Up.convert_to_project t.master (freshen t item)

  let convert_to_area t item =
    Up.convert_to_area t.master (freshen t item)

  let convert_to_action t item =
    Up.convert_to_action t.master (freshen t item)

  let make_full_tree r =
    let rec aux items =
      M.fold (fun _key item acc ->
        let adder =
          match item with
          | `Area _ as item -> Some (TreeNode.Add_project (Some item, `Active))
          | `Project _ as item -> Some (TreeNode.Add_action (Some item, None, None, `Next))
          | `Action _ -> None in
        let children = aux (R.child_nodes item) in
        acc |> TreeNode.add (TreeNode.unique_of_node ?adder ~children item)
      ) items TreeNode.Child_map.empty in
    aux (R.roots r)

  let make_process_tree r =
    let rec aux items =
      M.fold (fun _key item acc ->
        match item with
        | `Action _ -> acc
        | `Project _ as p when Node.project_state p <> `Active -> acc
        | `Area _ as item -> add_group (TreeNode.Add_project (Some item, `Active)) item acc
        | `Project _ as item -> add_group (TreeNode.Add_action (Some item, None, None, `Next)) item acc
      ) items TreeNode.Child_map.empty
    and add_group adder item acc =
      let children = aux (R.child_nodes item) in
      acc |> TreeNode.add (TreeNode.unique_of_node ~adder ~children item) in
    aux (R.roots r)

  let make_waiting_tree r =
    let waiting =
      let adder = TreeNode.Add_action (None, None, None, `Waiting) in
      TreeNode.group ~adder ~pri:(-1) "(unspecified reason)" in
    let results = ref TreeNode.Child_map.empty in
    let add ~group:contact ~parent item =
      let parent = parent >|?= (fun p ->
        let adder =
          match contact.TreeNode.adder with
          | Some (TreeNode.Add_action (None, ctx, contact, state)) ->
              Some (TreeNode.Add_action (parent, ctx, contact, state))
          | _ -> None in
        TreeNode.group_of_node ?adder p
      ) in
      TreeNode.add_grouped
        ~top:results
        ~groups:[Some contact; parent]
        (TreeNode.unique_of_node item) in
    let rec scan ?parent _key = function
      | `Area _ | `Project _ as node ->
          R.child_nodes node |> M.iter (scan ~parent:node)
      | `Action _ as node ->
          match Node.action_state node with
          | `Waiting -> add ~group:waiting ~parent node
          | `Waiting_for_contact ->
              begin match R.contact_for node with
              | Some contact ->
                  let adder = TreeNode.Add_action (None, None, Some contact, `Waiting_for_contact) in
                  add ~group:(TreeNode.unique_of_node ~adder contact) ~parent node
              | None -> add ~group:waiting ~parent node (* Shouldn't happen *)
              end
          | _ -> () in
    R.roots r |> M.iter (scan ?parent:None);
    !results

  let make_future_tree r =
    let add ~parent item lst =
      let parent = parent >|?= (fun p ->
        let adder =
          match item with
          | `Project _ -> TreeNode.Add_project (parent, `SomedayMaybe)
          | `Action _ -> TreeNode.Add_action (parent, None, None, `Future) in
        TreeNode.group_of_node ~adder p
      ) in
      TreeNode.add_grouped
        ~top:lst
        ~groups:[parent]
        (TreeNode.unique_of_node item) in
    let projects = ref TreeNode.Child_map.empty in
    let actions = ref TreeNode.Child_map.empty in
    let rec scan ?parent _key = function
      | `Project _ as node when Node.project_state node = `SomedayMaybe ->
          projects |> add ~parent node
      | `Area _ | `Project _ as node ->
          R.child_nodes node |> M.iter (scan ~parent:node)
      | `Action _ as node ->
          match Node.action_state node with
          | `Future -> actions |> add ~parent node
          | _ -> () in
    R.roots r |> M.iter (scan ?parent:None);
    let add_action = TreeNode.Add_action (None, None, None, `Future) in
    let add_project = TreeNode.Add_project (None, `SomedayMaybe) in
    TreeNode.Child_map.empty
    |> TreeNode.add (TreeNode.group ~pri:0 ~adder:add_action ~children:!actions "Actions")
    |> TreeNode.add (TreeNode.group ~pri:0 ~adder:add_project ~children:!projects "Projects")

  let make_areas_tree r =
    let rec aux items =
      M.fold (fun _key item acc ->
        match item with
        | `Area _ as item ->
            let adder = TreeNode.Add_project (Some item, `Active) in
            let children = aux (R.child_nodes item) in
            acc |> TreeNode.add (TreeNode.unique_of_node ~adder ~children item)
        | `Project _ | `Action _ -> acc
      ) items TreeNode.Child_map.empty in
    aux (R.roots r)

  let make_done_tree r =
    let rec aux items =
      M.fold (fun _key item acc ->
        match item with
        | `Area _ ->
            let children = aux (R.child_nodes item) in
            if TreeNode.Child_map.is_empty children then acc
            else acc |> TreeNode.add (TreeNode.unique_of_node ~children item)
        | `Project _ as p ->
            let children = aux (R.child_nodes item) in
            let p_done = Node.project_state p = `Done in
            if TreeNode.Child_map.is_empty children && not p_done then acc
            else acc |> TreeNode.add (TreeNode.unique_of_node ~children item)
        | `Action _ as a ->
            if Node.action_state a = `Done then acc |> TreeNode.add (TreeNode.unique_of_node item)
            else acc
      ) items TreeNode.Child_map.empty in
    aux (R.roots r)

  let make_everything_tree r =
    let main = make_full_tree r in
    let contexts =
      Ck_id.M.fold (fun _key item acc ->
        acc |> TreeNode.add (TreeNode.unique_of_node item)
      ) (R.contexts r) TreeNode.Child_map.empty in
    let contacts =
      Ck_id.M.fold (fun _key item acc ->
        acc |> TreeNode.add (TreeNode.unique_of_node item)
      ) (R.contacts r) TreeNode.Child_map.empty in
    TreeNode.Child_map.empty
    |> TreeNode.(add (group ~adder:(Add_area None) ~pri:0 ~children:main "Areas"))
    |> TreeNode.(add (group ~adder:Add_context ~pri:1 ~children:contexts "Contexts"))
    |> TreeNode.(add (group ~adder:Add_contact ~pri:2 ~children:contacts "Contacts"))

  let make_review_tree ~mode r =
    match mode with
    | `Done -> make_done_tree r
    | `Waiting -> make_waiting_tree r
    | `Future -> make_future_tree r
    | `Areas -> make_areas_tree r
    | `Everything -> make_everything_tree r

  let make_contact_tree r =
    R.contacts r |> TreeNode.of_id_map (fun item ->
      let children =
        R.nodes_of_contact item
        |> List.filter (function
          | `Action _ as a -> Node.action_state a = `Waiting_for_contact
          | _ -> false
        )
        |> TreeNode.(of_list unique_of_node) in
      let adder = TreeNode.Add_action (None, None, Some item, `Waiting_for_contact) in
      TreeNode.unique_of_node ~children ~adder item
    )

  let make_schedule_tree r =
    let day = ref None in
    let results = ref TreeNode.Child_map.empty in
    R.schedule r |> List.iter (fun action ->
      match Node.action_state action with
      | `Waiting_until time ->
          let date = Ck_time.string_of_user_date time in
          let group =
            match !day with
            | Some (prev_date, group) when prev_date = date -> group
            | _ -> TreeNode.group ~pri:0 date in
          let node = TreeNode.unique_of_node action in
          let group = {group with TreeNode.children = TreeNode.add node group.TreeNode.children} in
          day := Some (date, group);
          results := TreeNode.add group !results
      | _ -> assert false
    );
    !results

  let is_someday_project p =
    Node.project_state p = `SomedayMaybe

  let fmt_problem = function
    | `No_next_action -> "Active project with no next action"
    | `Unread_conflicts -> "Unread merge conflicts report"
    | `Incomplete_child -> "Completed project with incomplete child"

  let get_problems xs =
    let parent = ref TreeNode.Child_map.empty in
    xs |> List.iter (fun (node, problem) ->
      let msg = fmt_problem problem in
      let adder =
        match problem, node with
        | `No_next_action, (`Project _ as p) -> Some (TreeNode.Add_action (Some p, None, None, `Next))
        | _ -> None in
      TreeNode.add_grouped ~top:parent
        ~groups:[Some (TreeNode.group ~pri:0 msg)]
        (TreeNode.group_of_node ?adder node)
    );
    !parent

  let make_work_tree ~hidden_areas r =
    let hidden_areas = !hidden_areas
      |> Ck_id.S.filter (fun uuid ->
          match R.get r uuid with
          | Some (`Area _ as a) when Node.parent a = None -> true
          | _ -> false
        ) in
    let filter = 
      M.fold (fun _key item acc ->
        match item with
        | `Area _ as a ->
            let hidden = Ck_id.S.mem (R.Node.uuid a) hidden_areas in
            (a, not hidden) :: acc
        | _ -> acc
      ) (R.roots r) []
      |> List.rev in
    let next_actions = ref TreeNode.Child_map.empty in
    let add_next context parent item =
      let context_item =
        match context with
        | None -> TreeNode.group ~pri:(-1) "(no context)"
        | Some c -> TreeNode.unique_of_node c in
      let parent_item = parent >|?= (fun p ->
        TreeNode.group_of_node ~adder:(TreeNode.Add_action (Some p, context, None, `Next)) p
      ) in
      TreeNode.add_grouped ~top:next_actions
        ~groups:[Some context_item; parent_item]
        (TreeNode.unique_of_node item) in

    let done_items = ref TreeNode.Child_map.empty in
    let rec scan ?parent ~in_hidden ~in_someday nodes =
      nodes |> M.iter (fun _k node ->
        match node with
        | `Project _ as project when Node.project_state project = `Done ->
            if not in_hidden then (
              let item = TreeNode.unique_of_node node in
              done_items := !done_items |> TreeNode.add item
            )
        | `Project _ as node ->
            let in_someday = in_someday || is_someday_project node in
            R.child_nodes node |> scan ~parent:node ~in_someday ~in_hidden
        | `Area _ as node ->
            let in_hidden = in_hidden || Ck_id.S.mem (Node.uuid node) hidden_areas in
            R.child_nodes node |> scan ~parent:node ~in_someday ~in_hidden
        | `Action _ as action ->
            let add () =
              add_next (R.context action) parent node in
            match Node.action_state action with
            | `Next when not (in_someday || in_hidden) -> add ()
            | `Waiting_until _ when Node.is_due action -> add ()
            | `Done when not in_hidden ->
                let item = TreeNode.unique_of_node node in
                done_items := !done_items |> TreeNode.add item
            | _ -> ()
      ) in
    scan ?parent:None ~in_someday:false ~in_hidden:false (R.roots r);
    let tree =
      TreeNode.Child_map.empty
      |> TreeNode.(add (group ~pri:(-1) ~children:(get_problems (R.problems r)) "Problems"))
      |> TreeNode.(add (group ~pri:0 ~children:!next_actions "Next actions"))
      |> TreeNode.(add (group ~pri:1 ~children:!done_items "Recently completed")) in
    (filter, tree)

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

  let contact_group ~pri ?parent ?context = function
    | None -> failwith "No contact!"
    | Some contact ->
        let open TreeNode in
        let title = "Waiting for " ^ Node.name contact in
        group ~pri title ~adder:(Add_action (parent, context, Some contact, `Waiting_for_contact))

  let date_group ~pri ?parent ?context state =
    let open TreeNode in
    let `Waiting_until date = state in
    let title = "Waiting until " ^ Ck_time.string_of_user_date date in
    group ~pri title ~adder:(Add_action (parent, context, None, (state :> action_state)))

  let group_by_type ~parent child_nodes =
    let tree_nodes = ref TreeNode.Child_map.empty in
    let group_for node =
      let open TreeNode in
      match parent, node with
      | `Area _ as parent, `Area _ -> group ~pri:0 "Sub-areas" ~adder:(Add_area (Some parent))
      | `Action _, _
      | _, `Area _ -> group ~pri:(-1) "Invalid"
      | `Area _ as parent, `Project _ -> group ~pri:1 "Projects" ~adder:(Add_project (Some parent, `Active))
      | `Project _ as parent, `Project _ -> group ~pri:1 "Sub-projects" ~adder:(Add_project (Some parent, `Active))
      | (`Area _ | `Project _ as parent), (`Action _ as a) ->
          let state = Node.action_state a in
          let adder = Add_action (Some parent, None, None, state) in
          match state with
          | `Next -> group ~pri:2 "Next actions" ~adder
          | `Waiting ->
              group ~pri:3 "Waiting actions" ~adder:(Add_action (Some parent, None, None, `Waiting))
          | `Waiting_for_contact -> contact_group ~pri:4 ~parent (R.contact_for a)
          | `Waiting_until _ as s -> date_group ~pri:5 ~parent s
          | `Future -> group ~pri:6 "Future actions" ~adder
          | `Done -> group ~pri:7 "Completed actions" in
    let add node =
      TreeNode.add_grouped ~top:tree_nodes
        ~groups:[Some (group_for node)]
        (TreeNode.unique_of_node node) in
    child_nodes |> M.iter (fun _k v -> add v);
    !tree_nodes

  let empty_details =
    {
      details_item = React.S.const None;
      details_parent = React.S.const None;
      details_context = React.S.const None;
      details_contact = None;
      details_children = ReactiveData.RList.empty;
      details_stop = ignore
    }

  let on_remove r = function
    | `Area _ | `Project _ | `Action _ as node ->
        (R.get r (Node.uuid node) :> Node.generic option)
    | `Context _ as node ->
        R.get_context r (Node.uuid node)
    | `Contact _ as node ->
        R.get_contact r (Node.uuid node)

  let context_details ~details_stop rs initial_node =
    let uuid = R.Node.uuid initial_node in
    let child_nodes context =
      let top = ref TreeNode.Child_map.empty in
      R.actions_of_context context
      |> List.iter (fun child ->
        let open TreeNode in
        let state = Node.action_state child in
        let adder = Add_action (None, Some context, None, state) in
        let group =
          match state with
          | `Next -> group ~pri:2 "Next actions" ~adder
          | `Waiting -> group ~pri:3 "Waiting actions" ~adder:(Add_action (None, Some context, None, `Waiting))
          | `Waiting_for_contact -> contact_group ~pri:4 ~context (R.contact_for child)
          | `Waiting_until _ as s -> date_group ~pri:5 ~context s
          | `Future -> group ~pri:6 "Future actions" ~adder
          | `Done -> group ~pri:7 "Completed actions" in
        TreeNode.add_grouped ~top ~groups:[Some group] (TreeNode.unique_of_node child)
      );
      !top in
    let tree = WidgetTree.make (child_nodes initial_node) in
    let details_item = rs |> React.S.map ~eq:opt_node_equal (fun r ->
      let item = R.get_context r uuid in
      let children =
        match item with
        | None -> TreeNode.Child_map.empty
        | Some item -> child_nodes item in
      WidgetTree.update tree children ~on_remove:(on_remove r);
      item
    ) in
    { empty_details with
      details_item;
      details_children = WidgetTree.widgets tree;
      details_stop;
    }

  let contact_details ~details_stop rs initial_node =
    let uuid = R.Node.uuid initial_node in
    let child_nodes c =
      let tree = ref TreeNode.Child_map.empty in
      let make_group ~pri name =
        let group = TreeNode.group ~pri name in
        tree := !tree |> TreeNode.add group;
        group in
      let responsible_for = lazy (make_group ~pri:1 "Responsible for") in
      let contact_for = lazy (make_group ~pri:2 "Contact for") in
      R.nodes_of_contact c
      |> List.iter (fun node ->
        let group =
          match node with
          | `Action _ as a when Node.action_state a = `Waiting_for_contact -> responsible_for
          | _ -> contact_for in
        TreeNode.add_grouped ~top:tree
          ~groups:[Some (Lazy.force group)]
          (TreeNode.unique_of_node node)
      );
      !tree in
    let tree = WidgetTree.make (child_nodes initial_node) in
    let details_item = rs |> React.S.map ~eq:opt_node_equal (fun r ->
      let item = R.get_contact r uuid in
      let children =
        match item with
        | None -> TreeNode.Child_map.empty
        | Some item -> child_nodes item in
      WidgetTree.update tree children ~on_remove:(on_remove r);
      item
    ) in
    { empty_details with
      details_item;
      details_children = WidgetTree.widgets tree;
      details_stop;
    }

  let apa_details ~details_stop rs initial_node =
    let uuid = R.Node.uuid initial_node in
    let child_nodes node =
      R.child_nodes node
      |> group_by_type ~parent:node in
    let tree = WidgetTree.make (child_nodes initial_node) in
    let all = rs |> React.S.map ~eq:assume_changed (fun r ->
      let item = R.get r uuid in
      let item, parent, context, contact, children =
        match item with
        | None -> None, None, None, None, TreeNode.Child_map.empty
        | Some item ->
            let contact = R.contact_for item in
            let context =
              match item with
              | `Area _ | `Project _ -> None
              | `Action _ as action -> Some (R.context action) in
            Some (item :> Item.generic), R.parent r item, context, contact, child_nodes item in
      WidgetTree.update tree children ~on_remove:(on_remove r);
      (item, parent, context, contact)
    ) in
    {
      details_item    = (all |> React.S.map ~eq:opt_node_equal (fun (item, _parent, _context, _contact) -> item));
      details_parent  = (all |> React.S.map ~eq:opt_node_equal (fun (_item, parent, _context, _contact) -> parent));
      details_context = (all |> React.S.map ~eq:opt_opt_node_equal (fun (_item, _parent, context, _contact) -> context));
      details_contact = Some (all |> React.S.map ~eq:opt_node_equal (fun (_item, _parent, _context, contact) -> contact));
      details_children = WidgetTree.widgets tree;
      details_stop;
    }

  let details t initial_node =
    let initial_node = (initial_node :> Node.generic) in
    let uuid = Node.uuid initial_node in
    try fst (Ck_id.M.find uuid t.details)
    with Not_found ->
      let details_stop () =
        t.details <- t.details |> Ck_id.M.remove uuid in
      (* Note: initial_node may already be out-of-date *)
      let (>|>) = function
        | None -> fun _ -> empty_details, ignore
        | Some x -> fun f ->
            let rs, set_r = React.S.create ~eq:R.equal t.r in
            f ~details_stop rs x, (fun r -> set_r r) in
      let details, update =
        match initial_node with
        | `Context _ -> R.get_context t.r uuid >|> context_details
        | `Contact _ -> R.get_contact t.r uuid >|> contact_details
        | `Area _ | `Project _ | `Action _ -> R.get t.r uuid >|> apa_details in
      t.details <- t.details |> Ck_id.M.add uuid (details, update);
      details

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
    let item = freshen t item in
    match item with
    | `Project _ | `Action _ as node -> candidate_parents_for_pa t node
    | `Area _ as node -> candidate_parents_for_a t node

  let candidate_contacts_for t item =
    let item = freshen t item in
    let contacts =
      R.contacts t.r |> Ck_id.M.bindings |> List.map (fun (_id, contact) ->
        (Node.name contact, fun () -> Up.set_contact t.master item (Some contact))
      )
      |> List.sort (fun a b -> compare (fst a) (fst b)) in
    let none = ("(no contact)", fun () -> Up.set_contact t.master item None) in
    none :: contacts

  let candidate_contexts_for t item =
    let item = freshen t item in
    let contexts =
      R.contexts t.r |> Ck_id.M.bindings |> List.map (fun (_id, context) ->
        (Node.name context, fun () -> Up.set_context t.master item (Some context))
      )
      |> List.sort (fun a b -> compare (fst a) (fst b)) in
    let none = ("(no context)", fun () -> Up.set_context t.master item None) in
    none :: contexts

  let rtree r fn =
    let rtree = WidgetTree.make (fn r) in
    let update_tree r =
      let on_remove node = (R.get r (Node.uuid node) :> Node.generic option) in
      WidgetTree.update rtree (fn r) ~on_remove in
    let widgets = WidgetTree.widgets rtree in
    (widgets, update_tree)

  let rec filters_eq a b =
    match a, b with
    | [], [] -> true
    | (x,xstate)::xs, (y,ystate)::ys ->
        Node.equal x y && xstate = ystate && filters_eq xs ys
    | _ -> false

  let filter_rtree r fn =
    let init_filter, init_tree = fn r in
    let filter, set_filter = React.S.create ~eq:filters_eq init_filter in
    let rtree = WidgetTree.make init_tree in
    let update r =
      let on_remove node = (R.get r (Node.uuid node) :> Node.generic option) in
      let new_filter, new_tree = fn r in
      set_filter new_filter;
      WidgetTree.update rtree new_tree ~on_remove in
    let widgets = WidgetTree.widgets rtree in
    ((filter, widgets), update)

  let get_log master =
    Up.branch_head master
    |> Git.Commit.history ~depth:100

  let make_tree r ~hidden_areas = function
    | `Process -> let t, u = rtree r make_process_tree in `Process t, u
    | `Work -> let t, u = filter_rtree r (make_work_tree ~hidden_areas) in `Work t, u
    | `Review mode -> let t, u = rtree r (make_review_tree ~mode) in `Review (mode, t), u
    | `Contact -> let t, u = rtree r make_contact_tree in `Contact t, u
    | `Schedule -> let t, u = rtree r make_schedule_tree in `Schedule t, u

  let set_mode t mode =
    let mode =
      match mode with
      | `Process | `Work | `Contact | `Schedule as m -> m
      | `Review -> `Review t.review_mode in
    let tree_view, update_tree = make_tree t.r ~hidden_areas:t.hidden_areas mode in
    t.update_tree <- update_tree;
    t.set_tree tree_view

  let set_review_mode t mode =
    t.review_mode <- mode;
    set_mode t `Review

  let tree t = t.tree
  let fix_head t entry =
    t.set_fixed_head entry;
    match entry with
    | None -> Up.fix_head t.master None
    | Some entry ->
        Git.Repository.commit t.repo entry.Git_storage_s.Log_entry.id
        >>= Up.fix_head t.master

  let fixed_head t = t.fixed_head

  let init_new_repo ~did_init repo =
    Git.Repository.empty repo >>= fun staging ->
    Ck_init.file_list |> Lwt_list.iter_p (fun path ->
      match Ck_init.read path with
      | None -> assert false
      | Some value ->
          let value =
            (* Move weekly review to next Sunday *)
            if path = "db/af66bb30-5488-4b8b-a171-ba4a048d6fd1" then (
              match Ck_disk_node.of_string value with
              | `Action _ as a ->
                  begin match Ck_disk_node.action_repeat a with
                  | None -> assert false
                  | Some repeat ->
                      let next = repeat |> Ck_time.next_repeat ~now:(Clock.now () |> Ck_time.of_unix_time) in
                      let a = Ck_disk_node.with_repeat a
                        (Some (Ck_time.(make_repeat ~from:next repeat.repeat_n repeat.repeat_unit))) in
                      let a = Ck_disk_node.with_astate a (`Waiting_until next) in
                      Ck_disk_node.to_string a end
              | _ -> assert false
            ) else value in
          let key = Irmin.Path.String_list.of_hum path in
          Git.Staging.update staging key value
    ) >>= fun () ->
    Git.Commit.commit staging ~msg:["Initialise repository"] >|= fun commit ->
    did_init := true;
    commit

  let init_repo ~did_init ?server ~server_branch repo =
    match server with
    | None -> init_new_repo ~did_init repo
    | Some base ->
        Client.fetch ~base ~server_branch >>= function
        | `Ok None -> init_new_repo ~did_init repo        (* Server is empty *)
        | `Ok (Some commit) -> return commit
        | `Cancelled_by_user -> failwith "Initial clone cancelled by user. Refresh to retry."
        | `Error msg -> failwith (Printf.sprintf "Failed to clone remote repository: %s" msg)

  let log_lock = Lwt_mutex.create ()

  let server_head t = t.server_head

  let enable_log t =
    assert (t.update_log = None);
    let log, set_log = React.S.create Git_storage_s.Log_entry_map.empty in
    t.update_log <- Some set_log;
    Lwt_mutex.with_lock log_lock (fun () ->
      get_log t.master >|= fun initial_log ->
      set_log initial_log;
      Slow_history.make ~delay:1.0 ~eq:Git_storage_s.Log_entry.equal log
      |> Delta_history.make
    )

  let disable_log t =
    match t.update_log with
    | None -> assert false
    | Some set_log ->
        set_log Git_storage_s.Log_entry_map.empty;
        t.update_log <- None

  let revert t entry =
    Up.revert ~repo:t.repo t.master entry

  let make ?(branch="master") ?server repo =
    let did_init = ref false in
    let on_update, set_on_update = Lwt.wait () in
    Git.Repository.branch repo Ck_client.tracking_branch >>= fun server_branch ->
    Git.Repository.branch ~if_new:(lazy (init_repo ~did_init ?server ~server_branch repo)) repo branch >>= fun master_branch ->
    Up.make ~on_update master_branch >>= fun master ->
    let server_head = Git.Branch.head server_branch >|~= function
      | None -> None
      | Some commit -> Some (Git.Commit.id commit) in
    let r = Up.head master in
    let alert, set_alert = React.S.create (R.alert r) in
    let fixed_head, set_fixed_head = React.S.create None in
    let hidden_areas = ref Ck_id.S.empty in
    let rtree, update_tree = make_tree r ~hidden_areas `Work in
    let tree, set_tree = React.S.create ~eq:assume_changed rtree in
    let client =
      match server with
      | None -> None
      | Some url ->
          let merge_from c = Up.sync master ~from:c in
          Some (Client.make ~master:master_branch ~server_branch ~merge_from url) in
    let t = {
      repo; master; r;
      tree; set_tree; update_tree;
      update_log = None;
      alert;
      fixed_head; set_fixed_head;
      details = Ck_id.M.empty;
      review_mode = `Done;
      keep_me = [];
      hidden_areas;
      client;
      server_head;
    } in
    Lwt.wakeup set_on_update (fun r ->
      set_alert (R.alert r);
      t.r <- r;
      t.details |> Ck_id.M.iter (fun _id (_, set) -> set r);
      t.update_tree r;
      if not (Up.fixed_head t.master) then set_fixed_head None;
      match t.update_log with
      | None -> return ()
      | Some set_log ->
          (* If we're still processing an [enable_log], finish that first *)
          Lwt_mutex.with_lock log_lock (fun () ->
            get_log master >|= set_log
          )
    );
    begin match client with
    | Some client when !did_init ->
        begin Client.sync client >|= function
        | `Ok () | `Cancelled_by_user -> ()
        | `Error msg -> failwith (Printf.sprintf "Initialised new repository, but failed to push changes: %s" msg)
        end
    | _ -> return ()
    end >>= fun () ->
    return t

  let alert t = t.alert

  let export_tar t =
    Up.head t.master |> R.commit |> Git.Commit.export_tar

  let search t ~n test =
    let n = ref n in
    let results = ref M.empty in
    let consider _id node =
      match test (node :> Item.generic) with
      | None -> ()
      | Some result ->
          results := !results |> M.add (R.Node.key node) result;
          decr n;
          if !n = 0 then raise Exit in
    begin try
      Ck_id.M.iter consider (R.nodes t.r);
      Ck_id.M.iter consider (R.contacts t.r);
      Ck_id.M.iter consider (R.contexts t.r);
    with Exit -> () end;
    !results

  let set_hidden t area h =
    let areas =
      !(t.hidden_areas)
      |> (if h then Ck_id.S.remove else Ck_id.S.add) (R.Node.uuid area)
      |> Ck_id.S.filter (fun uuid ->
          match R.get t.r uuid with
          | Some (`Area _ as a) when Node.parent a = None -> true
          | _ -> false  (* Clean up invalid areas *)
      ) in
    t.hidden_areas := areas;
    t.update_tree t.r

  let client t = t.client
end
