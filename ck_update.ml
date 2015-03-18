(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs
open Lwt

let async : (unit -> unit Lwt.t) -> unit = Lwt.async

module Make(Git : Git_storage_s.S)
           (Clock : Ck_clock.S)
           (R : sig
             include REV with type commit = Git.Commit.t
             val make : time:float -> Git.Commit.t -> t Lwt.t
             val disk_node : [< Node.generic] -> Ck_disk_node.generic
             val action_node : Node.Types.action_node -> Ck_disk_node.Types.action_node
             val project_node : Node.Types.project_node -> Ck_disk_node.Types.project_node
             val area_node : Node.Types.area_node -> Ck_disk_node.Types.area_node
           end) = struct
  open R.Node.Types

  type t = {
    branch : Git.Branch.t;
    mutable fixed_head : bool;            (* If [true], we are in "time-machine" mode, and not tracking [branch] *)
    mutable head : R.t;
    updated : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    mutable update_signal : unit React.S.t;
    on_update : (R.t -> unit Lwt.t) Lwt.t;   (* (a thread just to avoid a cycle at creation time) *)
    mutable alarm : unit Lwt.t;
  }

  let rec update_alarm t =
    Lwt.cancel (t.alarm);
    match R.expires t.head with
    | None -> ()
    | Some time ->
        let sleeper = Clock.sleep (time -. Clock.now ()) in
        t.alarm <- sleeper;
        async (fun () ->
          Lwt.catch
            (fun () ->
              sleeper >>= fun () ->
              Lwt_mutex.with_lock t.mutex (fun () ->
                update_head t (R.commit t.head)
              )
            )
            (function
              | Canceled -> return ()
              | ex -> raise ex
            )
        )
  and update_head t new_head =   (* Call with mutex locked *)
    let time = Clock.now () in
    R.make ~time new_head >>= fun new_head ->
    t.head <- new_head;
    t.on_update >>= fun on_update ->
    on_update new_head >|= fun () ->
    Lwt_condition.broadcast t.updated ();
    update_alarm t

  type update_cb = R.t -> unit Lwt.t

  let error fmt =
    Printf.ksprintf (fun msg -> `Error msg) fmt

  (* Must be called with t.mutex held *)
  let maybe_update_head t new_head =
    let old_head = R.commit t.head in
    match new_head with
    | None -> failwith "Branch has been deleted!"
    | Some new_head when Git.Commit.equal old_head new_head -> return ()
    | Some new_head -> update_head t new_head

  let make ~on_update branch =
    let mutex = Lwt_mutex.create () in
    match Git.Branch.head branch |> React.S.value with
    | None -> failwith "No commits on branch!"
    | Some initial_head ->
    let time = Clock.now () in
    R.make ~time initial_head >>= fun initial_head ->
    let updated = Lwt_condition.create () in
    let update_scheduled = ref false in
    let t = {
      branch;
      fixed_head = false;
      head = initial_head;
      updated;
      mutex;
      update_signal = React.S.const ();
      on_update;
      alarm = Lwt.return ();
    } in
    t.update_signal <-
      Git.Branch.head branch |> React.S.map (fun _ ->
        if not (!update_scheduled) then (
          update_scheduled := true;
          async (fun () ->
            Lwt_mutex.with_lock mutex (fun () ->
              update_scheduled := false;
              if not t.fixed_head then (
                (* Head might have changed while we waited for the lock. *)
                React.S.value (Git.Branch.head branch)
                |> maybe_update_head t
              ) else return ()  (* Fixed head - ignore updates *)
            )
          )
        )
      );
    update_alarm t;
    return t

  let fix_head t = function
    | None ->
        Lwt_mutex.with_lock t.mutex (fun () ->
          t.fixed_head <- false;
          React.S.value (Git.Branch.head t.branch)
          |> maybe_update_head t
        )
    | Some _ as new_head ->
        Lwt_mutex.with_lock t.mutex (fun () ->
          t.fixed_head <- true;
          maybe_update_head t new_head
        )

  let head t = t.head
  let fixed_head t = t.fixed_head

  let branch_head t =
    match React.S.value (Git.Branch.head t.branch) with
    | None -> failwith "Branch has been deleted!"
    | Some commit -> commit

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
      (* Merge to branch tip, even if we're on a fixed head *)
      let old_head = branch_head t in
      Git.Commit.merge old_head pull_rq >>= function
      | `Conflict msg -> Ck_utils.error "Conflict during merge: %s (discarding change)" msg
      | `Ok merged when Git.Commit.equal old_head merged ->
          (* Our change had no effect, so there's nothing to do. *)
          return (return ())
      | `Ok merged ->
      (* Check that the merge is readable *)
      let time = Clock.now () in
      Lwt.catch (fun () -> R.make ~time merged >|= ignore)
        (fun ex -> Ck_utils.error "Change generated an invalid commit:\n%s\n\nThis is a BUG. The invalid change has been discarded."
          (Printexc.to_string ex)) >>= fun () ->
      Lwt_mutex.with_lock t.mutex (fun () ->
        (* At this point, head cannot contain our commit because we haven't merged it yet,
         * and no updates can happen while we hold the lock. *)
        let updated = Lwt_condition.wait t.updated in
        Git.Branch.fast_forward_to t.branch merged >|= fun merge_result ->
        (* If `Ok, [updated] cannot have fired yet because we still hold the lock. When it does
         * fire next, it must contain our update. It must fire soon, as head has changed. *)
        if merge_result = `Ok then (
          (* If we were on a fixed head then return to tracking master. Otherwise, the user won't
           * see the update. *)
          t.fixed_head <- false;
        );
        (merge_result, updated)
      ) >>= function
      | `Ok, updated -> return updated
      | `Not_fast_forward, _updated ->
          Log.warn "Update while we were trying to merge - retrying...";
          Clock.sleep 1.0 >>= fun () ->      (* Might be a bug - avoid hanging the browser *)
          (* Possibly we should wait for branch_head to move, but this is a very unlikely case
           * so do a simple sleep-and-retry *)
          aux ()
      in
    aux () >>= fun updated ->     (* Changes have been committed. *)
    updated >>= fun () ->         (* [on_update] has been called. *)
    return result

  let create t ~base ?uuid (node:[< Ck_disk_node.generic]) =
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
    let uuid = R.Node.uuid node in
    merge_to_master t ~base ~msg (fun view ->
      match new_disk_node with
      | `Area _ | `Project _ | `Action _ as new_disk_node ->
          assert (mem uuid base);
          let parent = Ck_disk_node.parent new_disk_node in
          if parent <> Ck_id.root && not (mem parent base) then
            Ck_utils.error "Parent '%a' does not exist!" Ck_id.fmt parent;
          let s = Ck_disk_node.to_string new_disk_node in
          Git.Staging.update view ["db"; Ck_id.to_string uuid] s
      | `Contact new_disk_node ->
          assert (Ck_id.M.mem uuid (R.contacts base));
          let s = Ck_disk_node.contact_to_string new_disk_node in
          Git.Staging.update view ["contact"; Ck_id.to_string uuid] s
      | `Context new_disk_node ->
          assert (Ck_id.M.mem uuid (R.contexts base));
          let s = Ck_disk_node.context_to_string new_disk_node in
          Git.Staging.update view ["context"; Ck_id.to_string uuid] s
    )

  let delete t node =
    let uuid = R.Node.uuid node |> Ck_id.to_string in
    let base = R.Node.rev node in
    let msg = Printf.sprintf "Delete '%s'" (R.Node.name node) in
    let remove path =
      merge_to_master ~base ~msg t (fun view ->
        Git.Staging.remove view path
      ) >|= fun () ->
      `Ok () in
    match node with
    | `Contact _ -> remove ["contact"; uuid]
    | `Context _ -> remove ["context"; uuid]
    | `Area _ | `Project _ | `Action _ as node ->
        try
          let (_, child) = Ck_utils.M.min_binding (R.child_nodes node) in
          error "Can't delete because it has a child (%s)" (R.Node.name child) |> return
        with Not_found -> remove ["db"; uuid]

  let add t ?uuid ~parent maker =
    let base, parent =
      match parent with
      | `Toplevel rev -> (rev, Ck_id.root)
      | `Node p -> (R.Node.rev p, R.Node.uuid p) in
    let disk_node =
      maker ~parent ~ctime:(Unix.gettimeofday ()) in
    create t ?uuid ~base disk_node

  let add_contact t ~base contact =
    let uuid = Ck_id.mint () in
    assert (not (Ck_id.M.mem uuid (R.contacts base)));
    let s = Ck_disk_node.contact_to_string contact in
    let msg = Printf.sprintf "Create '%s'" (Ck_disk_node.name (`Contact contact)) in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view ["contact"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let add_context t ~base context =
    let uuid = Ck_id.mint () in
    assert (not (Ck_id.M.mem uuid (R.contexts base)));
    let s = Ck_disk_node.context_to_string context in
    let msg = Printf.sprintf "Create '%s'" (Ck_disk_node.name (`Context context)) in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view ["context"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let set_name t node name =
    let msg = Printf.sprintf "Rename '%s' to '%s'" (R.Node.name node) name in
    update t ~msg node (Ck_disk_node.with_name (R.disk_node node) name)

  let set_description t node v =
    let msg = Printf.sprintf "Update description for '%s'" (R.Node.name node) in
    update t ~msg node (Ck_disk_node.with_description (R.disk_node node) v)

  let set_context t node context =
    let context =
      match context with
      | None -> None
      | Some context ->
          let context = `Context context in
          assert (R.Node.rev (`Action node) == R.Node.rev context);
          Some (R.Node.uuid context) in
    let new_node = Ck_disk_node.with_context (R.action_node node) context in
    let node = `Action node in
    let msg = Printf.sprintf "Change state of '%s'" (R.Node.name node) in
    update t ~msg node (`Action new_node)

  let set_action_state t node astate =
    let astate =
      match astate with
      | `Done | `Next | `Waiting | `Waiting_until _ | `Future as s -> s
      | `Waiting_for_contact contact ->
          let contact = `Contact contact in
          assert (R.Node.rev (`Action node) == R.Node.rev contact);
          `Waiting_for_contact (R.Node.uuid contact) in
    let new_node = Ck_disk_node.with_astate (R.action_node node) astate in
    let node = `Action node in
    let msg = Printf.sprintf "Change state of '%s'" (R.Node.name node) in
    update t ~msg node (`Action new_node)

  let set_project_state t node pstate =
    let new_node = Ck_disk_node.with_pstate (R.project_node node) pstate in
    let node = `Project node in
    let msg = Printf.sprintf "Change state of '%s'" (R.Node.name node) in
    update t ~msg node (`Project new_node)

  let set_starred t node s =
    let new_node =
      match node with
      | `Action a -> Ck_disk_node.with_starred (`Action (R.action_node a)) s
      | `Project p -> Ck_disk_node.with_starred (`Project (R.project_node p)) s in
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

  exception Found of [ area | project | action ]

  let is_area = function
    | `Area _ -> true
    | _ -> false

  let find_example_child pred node =
    try
      R.child_nodes node |> Ck_utils.M.iter (fun _ n -> if pred n then raise (Found n));
      None
    with Found x -> Some x

  let convert_to_project t node =
    let new_details =
      match node with
      | `Action a -> `Ok (Ck_disk_node.as_project (`Action (R.action_node a)))
      | `Area a ->
          match find_example_child is_area node with
          | None -> `Ok (Ck_disk_node.as_project (`Area (R.area_node a)))
          | Some subarea ->
              error "Can't convert to a project because it has a sub-area (%s)" (R.Node.name subarea)
    in
    match new_details with
    | `Error _ as e -> return e
    | `Ok new_details ->
    let msg = Printf.sprintf "Convert %s to project" (R.Node.name node) in
    update t ~msg node (`Project new_details) >|= fun () ->
    `Ok ()

  let convert_to_area t node =
    let new_details = `Area (Ck_disk_node.as_area (R.project_node node)) in
    let node = `Project node in
    match R.parent (R.Node.rev node) node with
    | Some p when not (is_area p) ->
        return (error "Can't convert to area because parent (%s) is not an area" (R.Node.name p))
    | _ ->
    let msg = Printf.sprintf "Convert %s to area" (R.Node.name node) in
    update t ~msg node new_details >|= fun () ->
    `Ok ()

  let convert_to_action t node =
    let new_details = `Action (Ck_disk_node.as_action (R.project_node node)) in
    let node = `Project node in
    try
      let (_, child) = Ck_utils.M.min_binding (R.child_nodes node) in
      error "Can't convert to an action because it has a child (%s)" (R.Node.name child) |> return
    with Not_found ->
    let msg = Printf.sprintf "Convert %s to action" (R.Node.name node) in
    update t ~msg node new_details >|= fun () ->
    `Ok ()
end
