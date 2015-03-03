(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs
open Lwt

let async : (unit -> unit Lwt.t) -> unit = Lwt.async

module Make(I : Irmin.BASIC with type key = string list and type value = string)
           (R : sig
             include REV with type V.db = I.t
             val disk_node : 'a Node.t -> 'a Ck_disk_node.t
           end) = struct
  type t = {
    branch : string -> I.t;
    mutable head : Irmin.Hash.SHA1.t;
    updated : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
  }

  type update_cb = Irmin.Hash.SHA1.t -> unit Lwt.t

  let get_head_commit branch =
    I.head (branch "Get latest commit") >>= function
    | Some commit -> return commit
    | None ->
        I.update (branch "Init") ["ck-version"] "0.1" >>= fun () ->
        I.head_exn (branch "Get initial commit")

  let make ~on_update branch =
    let watch_tags = I.watch_tags (branch "Watch master") in
    get_head_commit branch >>= fun initial_head ->
    let mutex = Lwt_mutex.create () in
    match I.branch (branch "Get branch name") with
    | `Head _ -> failwith "Not a tag!"
    | `Tag branch_name ->
    let t = {
      branch;
      head = initial_head;
      updated = Lwt_condition.create ();
      mutex;
    } in
    async (fun () ->
      on_update >>= fun on_update ->
      watch_tags |> Lwt_stream.iter_s (function
        | (n, Some _commit) when n = branch_name ->
            (* (ignore the commit ID in the update message; we want the latest) *)
            Lwt_mutex.with_lock t.mutex (fun () ->
              I.head (branch "Get latest commit") >>= function
              | None -> Ck_utils.error "Branch '%s' has disappeared!" branch_name
              | Some head ->
                  if head <> t.head then (
                    t.head <- head;
                    on_update head >>= fun () ->
                    Lwt_condition.broadcast t.updated ();
                    return ()
                  ) else (
                    return ()
                  )
            )
        | _ -> return ()
      )
    );
    return t

  let head t = t.head

  let mem uuid rev =
    R.get rev uuid <> None

  (* Branch from base, apply [fn branch] to it, then merge the result back to master.
   * Returns only once [on_update] has been run for the new revision. *)
  let merge_to_master t ~base ~msg fn =
    let path = I.Key.empty in
    R.make_view base >>= fun view ->
    fn view >>= fun result ->
    (* TODO: check if we can FF, and merge if not *)
    Lwt_mutex.with_lock t.mutex (fun () ->
      (* At this point, head cannot contain our commit because we haven't made it yet,
       * and no updates can happen while we hold the lock. *)
      let old_head = t.head in
      let updated : unit Lwt.t = Lwt_condition.wait t.updated in
      R.V.merge_path (t.branch msg) path view >>= function
      | `Conflict msg -> Ck_utils.error "Conflict during merge: %s (discarding change)" msg
      | `Ok () ->
      I.head (t.branch "Get latest commit") >|= function
      | Some new_head when new_head <> old_head ->
          (* [updated] cannot have fired yet because we still hold the lock. When it does
           * fire next, it must contain our update. It must fire soon, as head has changed. *)
          updated
      | _ ->
          (* Our change had no effect, so don't wait for head to move.
           * Or, the branch was deleted - no point waiting then either. *)
          return ()
    ) >>= fun updated ->    (* Changes have been committed. *)
    updated >>= fun () ->   (* [on_update] has been called. *)
    return result

  let create t ~base ?uuid (node:_ Ck_disk_node.t) =
    let uuid =
      match uuid with
      | Some uuid -> uuid
      | None -> Ck_id.mint () in
    assert (not (mem uuid base));
    let parent = Ck_disk_node.parent node in
    if parent <> Ck_id.root && not (mem parent base) then
      Ck_utils.error "Parent '%a' does not exist!" Ck_id.fmt parent;
    let s = Ck_disk_node.to_string node in
    let msg = Printf.sprintf "Create '%s'" (Ck_disk_node.name node) in
    merge_to_master t ~base ~msg (fun view ->
      R.V.update view ["db"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let update t ~msg node new_disk_node =
    let base = R.Node.rev node in
    assert (mem (R.Node.uuid node) base);
    let parent = R.Node.parent node in
    if parent <> Ck_id.root && not (mem parent base) then
      Ck_utils.error "Parent '%a' does not exist!" Ck_id.fmt (R.Node.parent node);
    let s = Ck_disk_node.to_string new_disk_node in
    merge_to_master t ~base ~msg (fun view ->
      R.V.update view ["db"; Ck_id.to_string (R.Node.uuid node)] s
    )

  let delete t node =
    let base = R.Node.rev node in
    let uuid = R.Node.uuid node in
    let msg = Printf.sprintf "Delete '%s'" (R.Node.name node) in
    merge_to_master ~base ~msg t (fun view ->
      R.V.remove view ["db"; Ck_id.to_string uuid]
    )

  let add t ?uuid details ~parent ~name ~description =
    let base, parent =
      match parent with
      | `Toplevel rev -> (rev, Ck_id.root)
      | `Node p -> (R.Node.rev p, R.Node.uuid p) in
    let disk_node =
      Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
    create t ?uuid ~base disk_node

  let set_name t node name =
    let msg = Printf.sprintf "Rename '%s' to '%s'" (R.Node.name node) name in
    update t ~msg node (Ck_disk_node.with_name (R.disk_node node) name)

  let set_details t node new_details =
    let new_node = Ck_disk_node.with_details (R.disk_node node) new_details in
    let msg = Printf.sprintf "Change state of '%s'" (R.Node.name node) in
    update t ~msg node new_node

  let set_action_state t node astate =
    match R.disk_node node with
    | { Ck_disk_node.details = `Action old; _ } -> set_details t node (`Action {old with astate})

  let set_project_state t node pstate =
    match R.disk_node node with
    | { Ck_disk_node.details = `Project old; _ } -> set_details t node (`Project {old with pstate})

  let set_starred t node s =
    let new_node =
      match R.disk_node node with
      | {Ck_disk_node.details = `Action d; _} as n -> Ck_disk_node.with_details n (`Action {d with astarred = s})
      | {Ck_disk_node.details = `Project d; _} as n -> Ck_disk_node.with_details n (`Project {d with pstarred = s}) in
    let action = if s then "Add" else "Remove" in
    let msg = Printf.sprintf "%s star for '%s'" action (R.Node.name node) in
    update t ~msg node new_node
end
