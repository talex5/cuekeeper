(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs
open Lwt

let async : (unit -> unit Lwt.t) -> unit = Lwt.async

module Make(Git : Git_storage_s.S)
           (R : sig
             include REV with type commit = Git.Commit.t
             val disk_node : 'a Node.t -> 'a Ck_disk_node.t
           end) = struct
  type t = {
    branch : Git.Branch.t;
    head : Git.Commit.t ref;
    updated : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    update_signal : unit React.S.t;
  }

  type update_cb = Git.Commit.t -> unit Lwt.t

  let make ~on_update branch =
    let mutex = Lwt_mutex.create () in
    match Git.Branch.head branch |> React.S.value with
    | None -> failwith "No commits on branch!"
    | Some initial_head ->
    let head = ref initial_head in
    let updated = Lwt_condition.create () in
    let update_scheduled = ref false in
    let update_signal =
      Git.Branch.head branch |> React.S.map (fun _ ->
        if not (!update_scheduled) then (
          update_scheduled := true;
          async (fun () ->
            Lwt_mutex.with_lock mutex (fun () ->
              update_scheduled := false;
              (* Head might have changed while we waited for the lock. *)
              match React.S.value (Git.Branch.head branch) with
              | None ->
                  failwith "Branch has been deleted!"
              | Some new_head when not (Git.Commit.equal !head new_head) ->
                  head := new_head;
                  on_update >>= fun on_update ->
                  on_update new_head >>= fun () ->
                  Lwt_condition.broadcast updated ();
                  return ()
              | Some _ ->
                  return ()  (* No change *)
            )
          )
        )
      ) in
    return {
      branch;
      head;
      updated;
      mutex;
      update_signal;
    }

  let head t = !(t.head)

  let mem uuid rev =
    R.get rev uuid <> None

  (* Branch from base, apply [fn branch] to it, then merge the result back to master.
   * Returns only once [on_update] has been run for the new revision. *)
  let merge_to_master t ~base ~msg fn =
    let base_commit = R.commit base in
    Git.Commit.checkout base_commit >>= fun view ->
    fn view >>= fun result ->
    Git.Commit.commit ~msg view >>= fun pull_rq ->
    let rec aux () =
      let old_head = head t in
      Git.Commit.merge old_head pull_rq >>= function
      | `Conflict msg -> Ck_utils.error "Conflict during merge: %s (discarding change)" msg
      | `Ok merged when Git.Commit.equal old_head merged ->
          (* Our change had no effect, so there's nothing to do. *)
          return (return ())
      | `Ok merged ->
      Lwt_mutex.with_lock t.mutex (fun () ->
        (* At this point, head cannot contain our commit because we haven't merged it yet,
         * and no updates can happen while we hold the lock. *)
        let updated = Lwt_condition.wait t.updated in
        Git.Branch.fast_forward_to t.branch merged >|= fun merge_result ->
        (* If `Ok, [updated] cannot have fired yet because we still hold the lock. When it does
         * fire next, it must contain our update. It must fire soon, as head has changed. *)
        (merge_result, updated)
      ) >>= function
      | `Ok, updated -> return updated
      | `Not_fast_forward, updated ->
          Log.warn "Update while we were trying to merge - retrying...";
          if old_head <> head t then aux ()
          else updated >>= aux
      in
    aux () >>= fun updated ->     (* Changes have been committed. *)
    updated >>= fun () ->         (* [on_update] has been called. *)
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
      Git.Staging.update view ["db"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let update t ~msg node new_disk_node =
    let base = R.Node.rev node in
    assert (mem (R.Node.uuid node) base);
    let parent = R.Node.parent node in
    if parent <> Ck_id.root && not (mem parent base) then
      Ck_utils.error "Parent '%a' does not exist!" Ck_id.fmt (R.Node.parent node);
    let s = Ck_disk_node.to_string new_disk_node in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view ["db"; Ck_id.to_string (R.Node.uuid node)] s
    )

  let delete t node =
    let base = R.Node.rev node in
    let uuid = R.Node.uuid node in
    let msg = Printf.sprintf "Delete '%s'" (R.Node.name node) in
    merge_to_master ~base ~msg t (fun view ->
      Git.Staging.remove view ["db"; Ck_id.to_string uuid]
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

  let set_pa_parent t node new_parent =
    assert (R.Node.rev node == R.Node.rev new_parent);
    let new_node = Ck_disk_node.with_parent (R.disk_node node) (R.Node.uuid new_parent) in
    let msg = Printf.sprintf "Move %s under %s" (R.Node.name node) (R.Node.name new_parent) in
    update t ~msg node new_node
  let set_a_parent = set_pa_parent

  let remove_parent t node =
    let new_node = Ck_disk_node.with_parent (R.disk_node node) Ck_id.root in
    let msg = Printf.sprintf "Move %s to top level" (R.Node.name node) in
    update t ~msg node new_node
end
