(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils
open Lwt

module Make(Git : Git_storage_s.S) (R : Ck_rev.S with type commit = Git.Commit.t) = struct
  open R.Node.Types

  let ok x = return (`Ok x)

  type 'a patch =
    [ `Add of 'a
    | `Remove of 'a
    | `Update of 'a * 'a ] Ck_id.M.t

  type diff = {
    nodes : [area | project | action] patch;
    contacts : contact patch;
    contexts : context patch;
  }

  module type KIND = sig
    type t
    type disk_t
    val dir : string
    val get : R.t -> Ck_id.t -> t option
    val to_disk : t -> disk_t
    val merge : ?base:disk_t -> theirs:disk_t -> disk_t -> disk_t
    val to_string : disk_t -> string
    val diff : diff -> t patch
    val with_conflict : string -> disk_t -> disk_t
    val equal : t -> t -> bool
  end

  module APA : KIND = struct
    type t = [area | project | action]
    type disk_t = [Ck_disk_node.Types.area | Ck_disk_node.Types.project | Ck_disk_node.Types.action]
    let dir = "db"
    let get = R.get
    let to_disk = R.apa_node
    let merge = Ck_disk_node.merge
    let to_string = Ck_disk_node.to_string
    let diff p = p.nodes
    let with_conflict = Ck_disk_node.with_conflict
    let equal = R.Node.equal
  end

  module Contact : KIND = struct
    type t = contact
    type disk_t = Ck_disk_node.Types.contact
    let dir = "contact"
    let get = R.get_contact
    let to_disk = R.contact_node
    let merge = Ck_disk_node.merge_contact
    let to_string = Ck_disk_node.contact_to_string
    let diff p = p.contacts
    let with_conflict = Ck_disk_node.with_conflict
    let equal = R.Node.equal
  end

  module Context : KIND = struct
    type t = context
    type disk_t = Ck_disk_node.Types.context
    let dir = "context"
    let get = R.get_context
    let to_disk = R.context_node
    let merge = Ck_disk_node.merge_context
    let to_string = Ck_disk_node.context_to_string
    let diff p = p.contexts
    let with_conflict = Ck_disk_node.with_conflict
    let equal = R.Node.equal
  end

  (* [diff_k base v] is a map with an entry for every changed node in base or v, saying how it changed. *)
  let diff_k base_nodes v_nodes =
    Ck_id.M.merge (fun _key o n ->
      match o, n with
      | None, None -> assert false
      | None, Some added -> Some (`Add added)
      | Some removed, None -> Some (`Remove removed)
      | Some old, Some current when R.Node.equal old current -> None
      | Some old, Some current -> Some (`Update (old, current))
    ) base_nodes v_nodes

  (* [diff ~base_rev v] is a diff from [base_rev] to [v]. If [base_rev] isn't given
   * then we assume it was empty and only generate [`Add] patches. *)
  let diff ?base_rev v =
    let base_nodes, base_contacts, base_contexts =
      match base_rev with
      | None -> Ck_id.M.empty, Ck_id.M.empty, Ck_id.M.empty
      | Some base -> R.nodes base, R.contacts base, R.contexts base in
    let nodes = diff_k base_nodes (R.nodes v) in
    let contacts = diff_k base_contacts (R.contacts v) in
    let contexts = diff_k base_contexts (R.contexts v) in
    { nodes; contacts; contexts }

  (** [merge_k k ~their_changes ~our_changes stage] updates [stage], which initially contains
   * "their" state, to include the changes of kind [k] in [our_changes]. *)
  let merge_k (module K : KIND) ~their_changes ~our_changes ~stage =
    K.diff our_changes
    |> Ck_id.M.bindings
    |> Lwt_list.iter_s (fun (uuid, our_patch) ->
      let save x = K.to_string x |> Git.Staging.update stage [K.dir; Ck_id.to_string uuid] in
      let their_patch =
        try Some (Ck_id.M.find uuid (K.diff their_changes))
        with Not_found -> None in
      match our_patch, their_patch with
      | (`Add node | `Update (_, node)), None ->
          (* The change was only on our side, so use it directly *)
          K.to_disk node |> save
      | `Update (_, ours), Some (`Remove _) ->
          K.to_disk ours
          |> K.with_conflict "Deleted and modified; keeping modified version"
          |> save
      | `Remove _, (None | Some (`Remove _)) ->
          Git.Staging.remove stage [K.dir; Ck_id.to_string uuid]
      | `Remove _, Some (`Update (_, theirs)) ->
          K.to_disk theirs
          |> K.with_conflict "Deleted and modified; keeping modified version"
          |> save
      | `Add ours, Some (`Add theirs) ->
          if K.equal ours theirs then return ()
          else K.merge ?base:None ~theirs:(K.to_disk theirs) (K.to_disk ours) |> save
      | `Update (base, ours), Some (`Update (_, theirs)) ->
          if K.equal ours theirs then return ()
          else K.merge ~base:(K.to_disk base) ~theirs:(K.to_disk theirs) (K.to_disk ours) |> save
      | `Add _, Some (`Update _ | `Remove _)
      | (`Update _ | `Remove _), Some (`Add _) ->
          (* Add implies it wasn't in the base, Update/Remove that it was *)
          assert false
    )

  (* Used to check for cycles in the parent relation *)
  type rooted = Rooted | Checking

  type required = {
    required_contacts : Ck_id.S.t;
    required_contexts : Ck_id.S.t;
  }

  (* After merging the area/project/action nodes, find out which contacts
   * and contexts they refer to so that we don't delete them.
   * For example, one branch might delete a contact while another makes that
   * the contact for an action.
   * Also, fixup any invalid parent links.
   *)
  let scan_merged_nodes staging =
    let required_contacts = ref Ck_id.S.empty in
    let required_contexts = ref Ck_id.S.empty in
    let nodes = Hashtbl.create 100 in
    (* Load all the nodes into [nodes] *)
    Git.Staging.list staging [APA.dir] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          Git.Staging.read_exn staging key >|= fun node ->
          Ck_disk_node.of_string node |> Hashtbl.add nodes uuid
      | _ -> assert false
    ) >>= fun () ->
    let get uuid =
      try Some (Hashtbl.find nodes uuid)
      with Not_found -> None in
    (* Look up the parent node. [None] if there is no parent
     * or the parent is missing. *)
    let parent node =
      let parent = Ck_disk_node.parent node in
      if parent <> Ck_id.root then (
        try Some (Hashtbl.find nodes parent)
        with Not_found -> None
      ) else None in
    let to_clear = ref [] in
    let ignore_node : [< Ck_disk_node.generic] -> unit = ignore in
    let clear_parent ~msg uuid node =
      let node =
        Ck_disk_node.with_parent node Ck_id.root
        |> Ck_disk_node.with_conflict msg in
      Hashtbl.replace nodes uuid node;
      to_clear := (uuid, node) :: !to_clear;
      node in
    let rooted = Hashtbl.create 100 in
    (* Check that following the parent links from this node leads to a root.
     * If there's a cycle, break it. *)
    let rec ensure_rooted uuid node =
      try
        match Hashtbl.find rooted uuid with
        | Checking -> clear_parent ~msg:"Removed parent due to cycle" uuid node
        | Rooted -> node    (* Already checked this one *)
      with Not_found ->
        let parent = Ck_disk_node.parent node in
        if parent = Ck_id.root then (
          Hashtbl.add rooted uuid Rooted;
          node
        ) else match get parent with
        | None ->
            (* Maybe prevent this case from happening? *)
            Hashtbl.replace rooted uuid Rooted;
            clear_parent ~msg:"Parent was deleted" uuid node
        | Some parent_node ->
            Hashtbl.add rooted uuid Checking;
            ensure_rooted parent parent_node |> ignore_node;
            Hashtbl.replace rooted uuid Rooted;
            node
        in
    (* Scan all nodes *)
    nodes |> Hashtbl.iter (fun uuid node ->
      (* Ensure reachable from root, breaking cycles if necessary *)
      let node = ensure_rooted uuid node in
      (* Note any required contact *)
      begin match Ck_disk_node.contact node with
      | None -> ()
      | Some contact -> required_contacts := !required_contacts |> Ck_id.S.add contact
      end;
      (* Note any required context *)
      begin match node with
      | `Action _ as node ->
          begin match Ck_disk_node.context node with
          | None -> ()
          | Some context -> required_contexts := !required_contexts |> Ck_id.S.add context end
      | _ -> ()
      end;
      (* Check parent is of the right type *)
      match parent node, node with
      | Some `Action _, _ -> clear_parent ~msg:"Removed parent as it became an action" uuid node |> ignore_node
      | Some `Project _, `Area _ -> clear_parent ~msg:"Removed parent as it is not an area" uuid node |> ignore_node
      | _ -> ()
    );
    (* Apply all the changes now (since Hashtbl.iter isn't async). *)
    !to_clear |> Lwt_list.iter_s (fun (uuid, node) ->
      Ck_disk_node.to_string node
      |> Git.Staging.update staging [APA.dir; Ck_id.to_string uuid]
    ) >|= fun () ->
    {
      required_contacts = !required_contacts;
      required_contexts = !required_contexts;
    }

  (** If an node we require was deleted then bring it back.
   * This can only happen if it was deleted in one branch and untouched in the other. *)
  let keep (module K : KIND) ~base_rev required stage =
    let get uuid = K.get base_rev uuid >|?= K.to_disk in
    Ck_id.S.elements required
    |> Lwt_list.iter_s (fun uuid ->
        let path = [K.dir; Ck_id.to_string uuid] in
        Git.Staging.mem stage path >>= function
        | true -> return ()
        | false ->
            match get uuid with
            | None -> bug "Keep node '%a' doesn't exist in base!" Ck_id.fmt uuid
            | Some node ->
                node
                |> K.with_conflict "Deleted but also referenced; keeping"
                |> K.to_string
                |> Git.Staging.update stage path
    )

  let merge ?base ~theirs ours =
    match base with
    | Some base when Git.Commit.equal base theirs -> ok ours  (* The common case *)
    | _ ->
    Git.Commit.checkout theirs >>= fun stage ->
    let time = Ck_time.make ~year:2000 ~month:0 ~day:1 in
    begin match base with
    | None -> return None
    | Some base -> R.make ~time base >|= fun r -> Some r
    end >>= fun base_rev ->
    R.make ~time theirs >>= fun their_rev ->
    R.make ~time ours >>= fun our_rev ->
    let their_changes = diff ?base_rev their_rev in
    let our_changes = diff ?base_rev our_rev in
    merge_k (module APA) ~their_changes ~our_changes ~stage >>= fun () ->
    (* Now we know which nodes we're keeping, break any invalid parent links. *)
    scan_merged_nodes stage >>= fun required ->
    merge_k (module Context) ~their_changes ~our_changes ~stage >>= fun () ->
    merge_k (module Contact) ~their_changes ~our_changes ~stage >>= fun () ->
    begin match base_rev with
    | None -> return () (* If there's no base, there can't be anything in it to keep *)
    | Some base_rev ->
        keep (module Contact) ~base_rev required.required_contacts stage >>= fun () ->
        keep (module Context) ~base_rev required.required_contexts stage
    end >>= fun () ->
    (* We could perhaps avoid a merge here if stage = theirs, but probably not worth it. *)
    Git.Commit.commit ~parents:[theirs; ours] stage ~msg:"Merge" >>= ok
end
