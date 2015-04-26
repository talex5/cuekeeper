(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils
open Lwt

let async : (unit -> unit Lwt.t) -> unit = Lwt.async

(* Annoyingly, if you suspend the computer during a Lwt_js.sleep then the time spent suspended isn't
 * counted and we wake up too late. As a work-around, we do a quick check every 10s. See:
 * http://stackoverflow.com/questions/29656686/how-to-wait-until-a-given-time-even-when-laptop-is-suspended
 *)
let max_sleep_time = 10.0

module Make(Git : Git_storage_s.S) (Clock : Ck_clock.S) (R : Ck_rev.S with type commit = Git.Commit.t) = struct
  open R.Node.Types

  module Merge = Ck_merge.Make(Git)(R)

  type t = {
    branch : Git.Branch.t;
    mutable fixed_head : float option;       (* If [Some time], we are in "time-machine" mode, and not tracking [branch] *)
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
    | Some date ->
        let time = Ck_time.unix_time_of date in
        let delay = min max_sleep_time (time -. Clock.now ()) in
        let sleeper = Clock.sleep delay in
        t.alarm <- sleeper;
        async (fun () ->
          Lwt.catch
            (fun () ->
              sleeper >>= fun () ->
              if Clock.now () >= time then (
                Lwt_mutex.with_lock t.mutex (fun () ->
                  update_head t (R.commit t.head)
                )
              ) else (
                update_alarm t;
                return ()
              )
            )
            (function
              | Canceled -> return ()
              | ex -> raise ex
            )
        )
  and update_head t new_head =   (* Call with mutex locked *)
    let time =
      match t.fixed_head with
      | None -> Clock.now () |> Ck_time.of_unix_time
      | Some time -> Ck_time.of_unix_time time in
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
    let time = Clock.now () |> Ck_time.of_unix_time in
    R.make ~time initial_head >>= fun initial_head ->
    let updated = Lwt_condition.create () in
    let update_scheduled = ref false in
    let t = {
      branch;
      fixed_head = None;
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
              if t.fixed_head = None then (
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
          t.fixed_head <- None;
          React.S.value (Git.Branch.head t.branch)
          |> maybe_update_head t
        )
    | Some commit as new_head ->
        Lwt_mutex.with_lock t.mutex (fun () ->
          Git.Commit.task commit >>= fun task ->
          let time = Irmin.Task.date task |> Int64.to_float in
          t.fixed_head <- Some time;
          maybe_update_head t new_head
        )

  let head t = t.head
  let fixed_head t = t.fixed_head <> None

  let branch_head t =
    match React.S.value (Git.Branch.head t.branch) with
    | None -> failwith "Branch has been deleted!"
    | Some commit -> commit

  let mem uuid rev =
    R.get rev uuid <> None

  let ff_master t commit =
    (* Check that the commit is readable *)
    let time = Clock.now () |> Ck_time.of_unix_time in
    Lwt.catch (fun () -> R.make ~time commit >|= ignore)
      (fun ex -> bug "Change generated an invalid commit:\n%s\n\nThis is a BUG. The invalid change has been discarded."
        (Printexc.to_string ex)) >>= fun () ->
    Lwt_mutex.with_lock t.mutex (fun () ->
      (* At this point, head cannot contain our commit because we haven't merged it yet,
       * and no updates can happen while we hold the lock. *)
      let updated = Lwt_condition.wait t.updated in
      Git.Branch.fast_forward_to t.branch commit >|= fun merge_result ->
      (* If `Ok, [updated] cannot have fired yet because we still hold the lock. When it does
       * fire next, it must contain our update. It must fire soon, as head has changed. *)
      if merge_result = `Ok then (
        (* If we were on a fixed head then return to tracking master. Otherwise, the user won't
         * see the update. *)
        t.fixed_head <- None;
      );
      (merge_result, updated)
    )

  (* Branch from base, apply [fn branch] to it, then merge the result back to master.
   * Returns only once [on_update] has been run for the new revision. *)
  let merge_to_master t ~base ~msg fn =
    let base_commit = R.commit base in
    Git.Commit.checkout base_commit >>= fun view ->
    fn view >>= fun result ->
    Git.Commit.commit ~msg:[msg] view >>= fun pull_rq ->
    let rec aux () =
      (* Merge to branch tip, even if we're on a fixed head *)
      let old_head = branch_head t in
      Merge.merge ~base:base_commit ~theirs:old_head pull_rq >>= function
      | `Nothing_to_do ->
          (* Our change had no effect, so there's nothing to do. *)
          return (return ())
      | `Ok merged ->
          ff_master t merged >>= function
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

  let revert t ~repo log_entry =
    Merge.revert ~repo ~master:(branch_head t) log_entry >>= function
    | `Nothing_to_do -> return (`Ok ())
    | `Error _ as e -> return e
    | `Ok commit ->
        ff_master t commit >>= function
        | `Ok, updated -> updated >|= fun () -> `Ok ()
        | `Not_fast_forward, _updated ->
            return (error "Update while we were trying to revert - aborting")

  let sync t ~from:theirs =
    let ours = branch_head t in
    let ff commit =
      ff_master t commit >>= function
      | `Ok, updated -> updated >|= fun () -> `Ok ()
      | `Not_fast_forward, _updated ->
          return (error "Update while we were trying to sync - aborting") in
    let sync_with ~base =
      Merge.merge ?base ~theirs ours >>= function
      | `Nothing_to_do -> return (`Ok ())
      | `Ok merge -> ff merge in
    Git.Commit.lcas ours theirs >>= function
    | [] -> sync_with ~base:None
    | lcas ->
        if List.exists (Git.Commit.equal ours) lcas then ff theirs   (* Trivial - we have no changes *)
        else sync_with ~base:(Some (List.hd lcas))

  let create t ~base (node:[< Ck_disk_node.generic]) =
    let uuid = Ck_id.mint () in
    assert (not (mem uuid base));
    begin match Ck_disk_node.parent node with
    | Some parent when not (mem parent base) ->
        bug "Parent '%a' does not exist!" Ck_id.fmt parent;
    | _ -> () end;
    let s = Ck_disk_node.to_string node in
    let msg = Printf.sprintf "Create %s" (Ck_disk_node.name node) in
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
          begin match Ck_disk_node.parent new_disk_node with
          | Some parent when not (mem parent base) ->
              bug "Parent '%a' does not exist!" Ck_id.fmt parent;
          | _ -> () end;
          let s = Ck_disk_node.to_string new_disk_node in
          Git.Staging.update view ["db"; Ck_id.to_string uuid] s
      | `Contact _ as new_disk_node ->
          assert (Ck_id.M.mem uuid (R.contacts base));
          let s = Ck_disk_node.contact_to_string new_disk_node in
          Git.Staging.update view ["contact"; Ck_id.to_string uuid] s
      | `Context _ as new_disk_node ->
          assert (Ck_id.M.mem uuid (R.contexts base));
          let s = Ck_disk_node.context_to_string new_disk_node in
          Git.Staging.update view ["context"; Ck_id.to_string uuid] s
    )

  let delete t ?msg nodes =
    match nodes with
    | [] -> return (`Ok ())
    | x :: _ ->
        let base = R.Node.rev x in
        let msg =
          match msg with
          | None ->
              nodes
              |> List.map R.Node.name
              |> String.concat ", "
              |> Printf.sprintf "%s: deleted"
          | Some msg -> msg in
        let to_delete = nodes |> List.fold_left (fun acc node ->
            acc |> Ck_id.S.add (R.Node.uuid node)
          ) Ck_id.S.empty in
        (* We're trying to delete a node which is needed by each of [referrers]. Return one of them that
         * isn't also being deleted (if any) for the error message. *)
        let example_referrer referrers =
          try Some (referrers |> List.find (fun referrer -> not (Ck_id.S.mem (R.Node.uuid referrer) to_delete)))
          with Not_found -> None in
        let paths = nodes |> List.fold_left (fun acc node ->
          match acc with
          | `Error _ as e -> e
          | `Ok acc ->
              let uuid = R.Node.uuid node |> Ck_id.to_string in
              match node with
              | `Contact _ as node ->
                  begin match example_referrer (R.nodes_of_contact node) with
                  | None -> `Ok (["contact"; uuid] :: acc)
                  | Some r ->
                      error "Can't delete because referenced by '%s'" (R.Node.name r)
                  end
              | `Context _ as node ->
                  begin match example_referrer (R.actions_of_context node) with
                  | None -> `Ok (["context"; uuid] :: acc)
                  | Some r ->
                      error "Can't delete because referenced by '%s'" (R.Node.name r)
                  end
              | `Area _ | `Project _ | `Action _ as node ->
                  match example_referrer (R.child_nodes node |> Ck_utils.M.bindings |> List.map snd) with
                  | None -> `Ok (["db"; uuid] :: acc)
                  | Some child ->
                      error "Can't delete because of child '%s'" (R.Node.name child)
        ) (`Ok []) in
        match paths with
        | `Error _ as e -> return e
        | `Ok paths ->
            merge_to_master ~base ~msg t (fun view ->
              paths |> Lwt_list.iter_s (Git.Staging.remove view)
            ) >|= fun () ->
            `Ok ()

  let add t ~parent maker =
    let base, parent =
      match parent with
      | `Toplevel rev -> (rev, None)
      | `Node p -> (R.Node.rev p, Some (R.Node.uuid p)) in
    let disk_node =
      maker ?parent ~ctime:(Unix.gettimeofday ()) () in
    create t ~base disk_node

  let add_contact t ~base contact =
    let uuid = Ck_id.mint () in
    assert (not (Ck_id.M.mem uuid (R.contacts base)));
    let s = Ck_disk_node.contact_to_string contact in
    let msg = Printf.sprintf "Add contact %s" (Ck_disk_node.name contact) in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view ["contact"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let add_context t ~base context =
    let uuid = Ck_id.mint () in
    assert (not (Ck_id.M.mem uuid (R.contexts base)));
    let s = Ck_disk_node.context_to_string context in
    let msg = Printf.sprintf "Add context %s" (Ck_disk_node.name context) in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view ["context"; Ck_id.to_string uuid] s
    ) >|= fun () -> uuid

  let clear_conflicts t node =
    let msg = Printf.sprintf "%s: clear conflicts" (R.Node.name node) in
    update t ~msg node (Ck_disk_node.without_conflicts (R.disk_node node))

  let set_name t node name =
    let msg = Printf.sprintf "%s: rename to %s" (R.Node.name node) name in
    update t ~msg node (Ck_disk_node.with_name (R.disk_node node) name)

  let set_description t node v =
    let msg = Printf.sprintf "%s: update description" (R.Node.name node) in
    update t ~msg node (Ck_disk_node.with_description (R.disk_node node) v)

  let opt_name = function
    | None -> "unset"
    | Some n -> R.Node.name n

  let set_context t node context =
    let context_uuid =
      match context with
      | None -> None
      | Some context ->
          assert (R.Node.rev node == R.Node.rev context);
          Some (R.Node.uuid context) in
    let new_node = Ck_disk_node.with_context (R.action_node node) context_uuid in
    let msg = Printf.sprintf "%s: context now %s" (R.Node.name node) (opt_name context) in
    update t ~msg node new_node

  let set_contact t node contact =
    let contact_uuid =
      match contact with
      | None -> None
      | Some contact ->
          assert (R.Node.rev node == R.Node.rev contact);
          Some (R.Node.uuid contact) in
    let new_node = Ck_disk_node.with_contact (R.apa_node node) contact_uuid in
    let new_node =
      match new_node with
      | `Action _ as a when Ck_disk_node.action_state a = `Waiting_for_contact && contact_uuid = None ->
          let open Ck_disk_node.Types in
          (Ck_disk_node.with_astate a `Next :> [area | project | action])
      | _ -> new_node in
    let msg = Printf.sprintf "%s: contact now %s" (R.Node.name node) (opt_name contact) in
    update t ~msg node new_node

  let set_action_state t node astate =
    let astate = (astate :> Ck_sigs.action_state) in
    let new_node = Ck_disk_node.with_astate (R.action_node node) astate in
    (* When setting a repeating action to wait until a date, record the new date as the repeat date too. *)
    let new_node =
      match astate with
      | `Waiting_until date ->
          begin match Ck_disk_node.action_repeat new_node with
          | None -> new_node
          | Some r ->
              let new_r = Ck_time.(make_repeat ~from:date r.repeat_n r.repeat_unit) in
              Ck_disk_node.with_repeat new_node (Some new_r) end
      | _ -> new_node in
    let msg = Printf.sprintf "%s: %s -> %s"
      (R.Node.name node)
      (Ck_disk_node.fmt_astate (R.Node.action_state node))
      (Ck_disk_node.fmt_astate astate) in
    update t ~msg node new_node

  let set_repeat t node repeat =
    let open Ck_time in
    let new_node = Ck_disk_node.with_repeat (R.action_node node) repeat in
    let new_node =
      match repeat with
      | None -> new_node
      | Some r -> Ck_disk_node.with_astate new_node (`Waiting_until r.repeat_from) in
    let msg = Printf.sprintf "%s: repeat %s"
      (R.Node.name node)
      (match repeat with None -> "never" | Some r -> Ck_time.string_of_repeat r) in
    update t ~msg node new_node

  let set_waiting_for t node contact =
    assert (R.Node.rev node == R.Node.rev contact);
    let new_node = Ck_disk_node.with_astate (R.action_node node) `Waiting_for_contact in
    let new_node = Ck_disk_node.with_contact new_node (Some (R.Node.uuid contact)) in
    let msg = Printf.sprintf "%s: now waiting for %s" (R.Node.name node) (R.Node.name contact) in
    update t ~msg node new_node

  let set_project_state t node pstate =
    let new_node = Ck_disk_node.with_pstate (R.project_node node) pstate in
    let msg = Printf.sprintf "%s: %s -> %s"
      (R.Node.name node)
      (Ck_disk_node.fmt_pstate (R.Node.project_state node))
      (Ck_disk_node.fmt_pstate pstate) in
    update t ~msg node new_node

  let set_starred t node s =
    let new_node =
      match node with
      | `Action _ as a -> Ck_disk_node.with_starred (R.action_node a) s
      | `Project _ as p -> Ck_disk_node.with_starred (R.project_node p) s in
    let action = if s then "Add" else "Remove" in
    let msg = Printf.sprintf "%s: %s star" (R.Node.name node) action in
    update t ~msg node new_node

  let set_pa_parent t node new_parent =
    assert (R.Node.rev node == R.Node.rev new_parent);
    let new_node = Ck_disk_node.with_parent (R.apa_node node) (Some (R.Node.uuid new_parent)) in
    let msg = Printf.sprintf "%s: move under %s" (R.Node.name node) (R.Node.name new_parent) in
    update t ~msg node new_node
  let set_a_parent = set_pa_parent

  let remove_parent t node =
    let new_node = Ck_disk_node.with_parent (R.apa_node node) None in
    let msg = Printf.sprintf "%s: unset parent" (R.Node.name node) in
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
      | `Action _ as a -> `Ok (Ck_disk_node.as_project (R.action_node a))
      | `Area _ as a ->
          match find_example_child is_area node with
          | None -> `Ok (Ck_disk_node.as_project (R.area_node a))
          | Some subarea ->
              error "Can't convert to a project because it has a sub-area (%s)" (R.Node.name subarea)
    in
    match new_details with
    | `Error _ as e -> return e
    | `Ok new_details ->
    let msg = Printf.sprintf "Convert %s to project" (R.Node.name node) in
    update t ~msg node new_details >|= fun () ->
    `Ok ()

  let convert_to_area t node =
    let new_details = Ck_disk_node.as_area (R.project_node node) in
    match R.parent (R.Node.rev node) node with
    | Some p when not (is_area p) ->
        return (error "Can't convert to area because parent (%s) is not an area" (R.Node.name p))
    | _ ->
    let msg = Printf.sprintf "Convert %s to area" (R.Node.name node) in
    update t ~msg node new_details >|= fun () ->
    `Ok ()

  let convert_to_action t node =
    let new_details = Ck_disk_node.as_action (R.project_node node) in
    try
      let (_, child) = Ck_utils.M.min_binding (R.child_nodes node) in
      error "Can't convert to an action because it has a child (%s)" (R.Node.name child) |> return
    with Not_found ->
    let msg = Printf.sprintf "Convert %s to action" (R.Node.name node) in
    update t ~msg node new_details >|= fun () ->
    `Ok ()
end
