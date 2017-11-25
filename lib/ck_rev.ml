(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils

let root_id = Ck_id.of_string "" (* Convenient magic value for hashtables *)

module type S = sig
  include Ck_sigs.REV
  open Node.Types

  val make : time:Ck_time.user_date -> commit -> t Lwt.t
  val disk_node : [< Node.generic] -> Ck_disk_node.generic
  val apa_node : [< area | project | action] ->
    [ Ck_disk_node.Types.area | Ck_disk_node.Types.project | Ck_disk_node.Types.action ]

  val action_node : action -> Ck_disk_node.Types.action
  val project_node : project -> Ck_disk_node.Types.project
  val area_node : area -> Ck_disk_node.Types.area
  val context_node : context -> Ck_disk_node.Types.context
  val contact_node : contact -> Ck_disk_node.Types.contact
end

module Make(Git : Git_storage_s.S) = struct
  type commit = Git.Commit.t

  module Node = struct
    module Types = struct
      type rev = {
        commit : Git.Commit.t;
        mutable children : apa M.t Ck_id.M.t;
        contexts : context_node Ck_id.M.t ref;
        contacts : contact_node Ck_id.M.t ref;
        nodes_of_contact : (Ck_id.t, apa) Hashtbl.t;
        actions_of_context : (Ck_id.t, action) Hashtbl.t;
        apa_nodes : apa Ck_id.M.t ref;
        mutable alert : bool;
        mutable schedule : action list;
        mutable expires : Ck_time.user_date option;
        valid_from : Ck_time.user_date;
        mutable problems :
          ( [ `Action of action_node
            | `Project of project_node
            | `Area of area_node
            | `Contact of contact_node
            | `Context of context_node ] * Ck_sigs.problem
          ) list;
      }
      and 'a node_details = {
        rev : rev;
        uuid : Ck_id.t;
        disk_node : 'a;
      }
      and action_node = Ck_disk_node.Types.action_node node_details
      and project_node = Ck_disk_node.Types.project_node node_details
      and area_node = Ck_disk_node.Types.area_node node_details
      and context_node = Ck_disk_node.Types.context_node node_details
      and contact_node = Ck_disk_node.Types.contact_node node_details
      and apa =
        [ `Action of action_node
        | `Project of project_node
        | `Area of area_node ]
      and action = [`Action of action_node]
      and project = [`Project of project_node]
      and area = [`Area of area_node]
      and contact = [`Contact of contact_node]
      and context = [`Context of context_node]
    end

    open Types

    type generic =
      [ apa
      | `Contact of contact_node
      | `Context of context_node ]

    let apa_disk_node = function
      | `Action n -> `Action n.disk_node
      | `Project n -> `Project n.disk_node
      | `Area n -> `Area n.disk_node

    let disk_node = function
      | `Action n -> `Action n.disk_node
      | `Project n -> `Project n.disk_node
      | `Area n -> `Area n.disk_node
      | `Contact n -> `Contact n.disk_node
      | `Context n -> `Context n.disk_node

    let details = function
      | `Action n -> {n with disk_node = ()}
      | `Project n -> {n with disk_node = ()}
      | `Area n -> {n with disk_node = ()}
      | `Contact n -> {n with disk_node = ()}
      | `Context n -> {n with disk_node = ()}

    let rev n = (details n).rev
    let uuid n = (details n).uuid

    let contact t = Ck_disk_node.contact (apa_disk_node t)
    let parent t = Ck_disk_node.parent (apa_disk_node t)
    let name t = Ck_disk_node.name (disk_node t)
    let description t = Ck_disk_node.description (disk_node t)
    let ctime t = Ck_disk_node.ctime (disk_node t)
    let conflicts t = Ck_disk_node.conflicts (disk_node t)
    let action_state (`Action n) = Ck_disk_node.action_state (`Action n.disk_node)
    let action_repeat (`Action n) = Ck_disk_node.action_repeat (`Action n.disk_node)
    let context (`Action n) = Ck_disk_node.context (`Action n.disk_node)
    let project_state (`Project n) = Ck_disk_node.project_state (`Project n.disk_node)
    let starred = function
      | `Action n -> Ck_disk_node.starred (`Action n.disk_node)
      | `Project n -> Ck_disk_node.starred (`Project n.disk_node)
    let is_done = function
      | `Action n -> Ck_disk_node.is_done (`Action n.disk_node)
      | `Project n -> Ck_disk_node.is_done (`Project n.disk_node)

    let key node = (String.lowercase_ascii (name node), uuid node)

    let is_due action =
      match action_state action with
      | `Waiting_until time -> Ck_time.compare time (rev action).valid_from <= 0
      | _ -> false

    let node_due = function
      | `Action _ as action -> is_due action
      | _ -> false

    let equal a b =
      let a = (a :> generic) in
      let b = (b :> generic) in
      uuid a = uuid b &&
      disk_node a = disk_node b &&
      node_due a = node_due b   (* Force the GUI to update when an item becomes due *)
  end

  open Node.Types

  type rev = Node.Types.rev
  type t = rev

  let equal a b =
    Git.Commit.equal a.commit b.commit &&
    a.expires = b.expires

  let child_nodes node =
    let t = Node.rev node in
    let parent = Node.uuid node in
    try Ck_id.M.find parent t.children with Not_found -> M.empty

  let process_item ~now t node =
    begin match Node.contact node with
    | None -> ()
    | Some c ->
        if not (Ck_id.M.mem c !(t.contacts)) then
          bug "Contact '%a' of '%s' not found!" Ck_id.fmt c (Node.name node);
        Hashtbl.add t.nodes_of_contact c node end;
    match node with
    | `Action _ as node ->
        begin match Node.context node with
        | None -> ()
        | Some id ->
            if not (Ck_id.M.mem id !(t.contexts)) then
              bug "Context '%a' of '%s' not found!" Ck_id.fmt id (Node.name node);
            Hashtbl.add t.actions_of_context id node end;
        begin match Node.action_state node with
        | `Waiting_for_contact ->
            if Node.contact node = None then
              bug "Waiting_for_contact but no contact set on '%s'" (Node.name node)
        | `Waiting_until time ->
            t.schedule <- node :: t.schedule;
            if time <= now then t.alert <- true
            else (
              match t.expires with
              | Some old_time when Ck_time.compare old_time time <= 0 -> ()
              | _ -> t.expires <- Some time
            )
        | _ -> () end
    | _ -> ()

  type active = In_progress | Idle

  let is_area = function
    | `Area _ -> true
    | _ -> false

  let roots t =
    try Ck_id.M.find root_id t.children
    with Not_found -> M.empty

  let ensure_no_cycles t =
    let rec aux unseen nodes =
      M.fold (fun _key node unseen ->
        let unseen = unseen |> Ck_id.M.remove (Node.uuid node) in
        aux unseen (child_nodes node)
      ) nodes unseen in
    let unreachable = aux !(t.apa_nodes) (roots t) in
    if not (Ck_id.M.is_empty unreachable) then (
      let _id, example = Ck_id.M.min_binding unreachable in
      bug "Node '%s' is not reachable from the root! (cycle in parent relation)" (Node.name example)
    )

  let is_incomplete = function
    | `Area _ -> true
    | `Project _ | `Action _ as n -> not (Node.is_done n)

  let check_for_problems t =
    let add node msg =
      t.problems <- ((node :> Node.generic), msg) :: t.problems in
    let rec scan node =
      let bug problem =
        Ck_utils.bug "Bad item '%s': %s" (Node.name node) problem in
      let reduce_progress _ child acc =
        match scan child with
        | Idle -> acc
        | In_progress -> In_progress in
      if Node.conflicts node <> [] then add node `Unread_conflicts;
      let child_nodes = child_nodes node in
      match node with
      | `Project _ as node ->
          if (M.exists (fun _k -> is_area) child_nodes) then bug "Project with area child!";
          if Node.project_state node = `Done && M.exists (fun _k -> is_incomplete) child_nodes then
            add node `Incomplete_child;
          let children_status = M.fold reduce_progress child_nodes Idle in
          begin match Node.project_state node, children_status with
          | `Active, Idle -> add node `No_next_action; Idle
          | `Active, In_progress -> In_progress
          | (`SomedayMaybe | `Done), _ -> Idle end
      | `Area _ ->
          M.fold reduce_progress child_nodes Idle
      | `Action _ as node ->
          if not (M.is_empty child_nodes) then bug "Action with children!";
          begin match Node.action_state node with
          | `Next | `Waiting_for_contact | `Waiting_until _ | `Waiting -> In_progress
          | `Future -> Idle
          | `Done ->
              if Node.action_repeat node <> None then bug "Repeating action marked as done!";
              Idle end
      in
    let check_contact node =
      if Node.conflicts node <> [] then add node `Unread_conflicts in
    let check_context node =
      if Node.conflicts node <> [] then add node `Unread_conflicts in
    roots t |> M.iter (fun _k n -> ignore (scan n));
    !(t.contacts) |> Ck_id.M.iter (fun _k n -> check_contact (`Contact n));
    !(t.contexts) |> Ck_id.M.iter (fun _k n -> check_context (`Context n))

  let check_version tree =
    Git.Staging.read tree ["ck-version"] >|= function
    | None ->
        bug "'ck-version' file missing - repository is corrupted!"
    | Some v ->
        let v = String.trim v in
        if v <> "0.1" then
          bug "Unknown repository format version '%s' (expected 0.1) - please upgrade CueKeeper" v

  let make_no_cache ~time commit =
    Git.Commit.checkout commit >>= fun tree ->
    let contacts = ref Ck_id.M.empty in
    let contexts = ref Ck_id.M.empty in
    let children = Hashtbl.create 100 in
    let apa_nodes = ref Ck_id.M.empty in
    let nodes_of_contact = Hashtbl.create 10 in
    let actions_of_context = Hashtbl.create 10 in
    let t = {
      commit; contacts; apa_nodes; nodes_of_contact; children = Ck_id.M.empty;
      contexts; actions_of_context;
      schedule = []; alert = false; valid_from = time; expires = None;
      problems = [];
    } in
    check_version tree >>= fun () ->
    (* Load areas, projects and actions *)
    Git.Staging.list tree ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let disk_node = Ck_disk_node.of_string s in
          let node =
            match disk_node with
            | `Action disk_node -> `Action {rev = t; uuid; disk_node}
            | `Project disk_node -> `Project {rev = t; uuid; disk_node}
            | `Area disk_node -> `Area {rev = t; uuid; disk_node} in
          apa_nodes := !apa_nodes |> Ck_id.M.add uuid node;
          let parent = Ck_disk_node.parent disk_node |> default root_id in
          let old_children =
            try Hashtbl.find children parent
            with Not_found -> [] in
          Hashtbl.replace children parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    let apa_nodes = !apa_nodes in
    (* Load contacts *)
    Git.Staging.list tree ["contact"] >>=
    Lwt_list.iter_s (function
      | ["contact"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let `Contact disk_node = Ck_disk_node.contact_of_string s in
          let contact = {rev = t; uuid; disk_node} in
          contacts := !contacts |> Ck_id.M.add uuid contact;
      | _ -> assert false
    ) >>= fun () ->
    (* Load contexts *)
    Git.Staging.list tree ["context"] >>=
    Lwt_list.iter_s (function
      | ["context"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn tree key >|= fun s ->
          let `Context disk_node = Ck_disk_node.context_of_string s in
          let context = {rev = t; uuid; disk_node} in
          contexts := !contexts |> Ck_id.M.add uuid context;
      | _ -> assert false
    ) >>= fun () ->
    (* todo: reject cycles *)
    children |> Hashtbl.iter (fun parent children ->
      if parent <> root_id && not (Ck_id.M.mem parent apa_nodes) then (
        bug "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );
    t.children <-
      Hashtbl.fold (fun uuid child_uuids acc ->
        let child_map = child_uuids |> List.fold_left (fun acc child_uuid ->
          let child = Ck_id.M.find child_uuid apa_nodes in
          process_item ~now:time t child;
          acc |> M.add (Node.key child) child
        ) M.empty in
        acc |> Ck_id.M.add uuid child_map
      ) children Ck_id.M.empty;
    ensure_no_cycles t;
    check_for_problems t;
    if t.problems <> [] then t.alert <- true;
    return t

  let last = ref None
  let make ~time commit =
    let reload () =
      make_no_cache ~time commit >|= fun r ->
      last := Some r;
      r in
    match !last with
    | Some last when Git.Commit.equal last.commit commit ->
        begin match last.expires with
        | Some etime when time >= etime -> reload ()
        | _ -> return {last with valid_from = time} end
    | _ -> reload ()

  let nodes t = !(t.apa_nodes)

  let get t uuid =
    try Some (Ck_id.M.find uuid !(t.apa_nodes))
    with Not_found -> None

  let get_contact t uuid =
    try Some (`Contact (Ck_id.M.find uuid !(t.contacts)))
    with Not_found -> None

  let get_context t uuid =
    try Some (`Context (Ck_id.M.find uuid !(t.contexts)))
    with Not_found -> None

  let parent t node = Node.parent node >>?= get t

  let context (`Action node as action) =
    match Node.context action with
    | None -> None
    | Some id -> get_context node.rev id

  let commit t = t.commit
  let contacts t = !(t.contacts) |> Ck_id.M.map (fun c -> `Contact c)
  let contexts t = !(t.contexts) |> Ck_id.M.map (fun c -> `Context c)

  let nodes_of_contact (`Contact c) =
    Hashtbl.find_all c.rev.nodes_of_contact c.uuid

  let actions_of_context (`Context c) =
    Hashtbl.find_all c.rev.actions_of_context c.uuid

  let contact_for node =
    match Node.contact node with
    | None -> None
    | Some id -> get_contact (Node.rev node) id

  let disk_node = Node.disk_node
  let apa_node = Node.apa_disk_node
  let action_node (`Action n) = `Action n.disk_node
  let project_node (`Project n) = `Project n.disk_node
  let area_node (`Area n) = `Area n.disk_node
  let contact_node (`Contact n) = `Contact n.disk_node
  let context_node (`Context n) = `Context n.disk_node

  let due action =
    match Node.action_state action with
    | `Waiting_until time -> time
    | _ -> assert false

  let by_due_time a b =
    compare (due a) (due b)

  let schedule t = List.sort by_due_time t.schedule
  let alert t = t.alert
  let expires t = t.expires

  let problems t = List.rev (t.problems)
end
