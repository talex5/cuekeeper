(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt.Infix
open Ck_utils
open Ck_sigs

let tracking_branch = "server"

let (>>!=) x f =
  x >>= function
  | `Error _ as e -> Lwt.return e
  | `Ok y -> f y
  | `Cancelled_by_user as c -> Lwt.return c

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (RPC : RPC) = struct
  type t = {
    master : Git.Branch.t;
    server_branch : Git.Branch.t;
    merge_from : Git.Commit.t -> unit or_error Lwt.t;
    base : Uri.t;
    sync_in_progress : bool React.S.t;
    set_sync_in_progress : bool -> unit;
  }

  let make ~master ~server_branch ~merge_from base =
    let sync_in_progress, set_sync_in_progress = React.S.create false in
    {
      master; server_branch; merge_from; base;
      sync_in_progress; set_sync_in_progress;
    }

  let get ~base path =
    RPC.get (Uri.with_path base path) >>= function
    | `Cancelled_by_user -> Lwt.return `Cancelled_by_user
    | `Ok (resp, body) ->
    match resp.Cohttp.Response.status with
    | `OK -> Cohttp_lwt.Body.to_string body >|= fun body -> `Ok body
    | code -> Lwt.return (error "Bad status code '%s' from server" (Cohttp.Code.string_of_status code))

  let post ~base path body =
    let body = Cohttp_lwt.Body.of_string body in
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    RPC.post ~headers ~body (Uri.with_path base path) >>= function
    | `Cancelled_by_user -> Lwt.return `Cancelled_by_user
    | `Ok (resp, body) ->
    match resp.Cohttp.Response.status with
    | `OK -> Cohttp_lwt.Body.to_string body >|= fun body -> `Ok body
    | code -> Lwt.return (error "Bad status code '%s' from server" (Cohttp.Code.string_of_status code))

  let fetch ~base ~server_branch =
    let path =
      match React.S.value (Git.Branch.head server_branch) with
      | Some last_known -> "fetch/" ^ Digestif.SHA1.to_hex (Git.Commit.id last_known)
      | None -> "fetch" in
    get ~base path >>!= function
    | "" ->
        Git.Branch.force server_branch None >|= fun () -> `Ok None
    | bundle ->
        Git.Branch.fetch_bundle server_branch bundle >>!= fun commit ->
        Lwt.return (`Ok (Some commit))

  let pull t  =
    fetch ~base:t.base ~server_branch:t.server_branch >>!= function
    | None -> Lwt.return (`Ok None)
    | Some commit ->
    (* If server_head isn't in the history of master, merge it now. *)
    t.merge_from commit >>!= fun () -> Lwt.return (`Ok (Some commit))

  let push t server_head =
    match Git.Branch.head t.master |> React.S.value, server_head with
    | None, _ ->
        failwith "no master branch!"
    | Some new_head, Some server_head when Git.Commit.equal new_head server_head ->
        Lwt.return (`Ok ())
    | Some new_head, _ ->
        Git.Commit.bundle ~tracking_branch new_head >>= function
        | None -> Lwt.return (`Ok ())
        | Some bundle ->
        post ~base:t.base "push" bundle >>!= function
        | "ok" ->
            begin Git.Branch.fast_forward_to t.server_branch new_head >>= function
            | `Not_fast_forward ->
                Lwt.return (error "Push successful, but failed to fast-forward tracking branch - newer concurrent push?")
            | `Ok -> Lwt.return (`Ok ())
            end
        | "not-fast-forward" -> Lwt.return `Concurrent_update
        | msg -> Lwt.return (error "Unexpected response '%s'" msg)

  let sync t =
    let rec aux () =
      pull t >>!= fun server_head ->
      (* Our master branch now includes [server_head] *)
      push t server_head >>= function
      | `Concurrent_update ->
          Printf.eprintf "Warning: Concurrent update during sync; retrying\n%!";
          Clock.sleep 2.0 >>= aux
      | `Ok () | `Cancelled_by_user | `Error _ as r -> Lwt.return r in
    if React.S.value t.sync_in_progress then
      Lwt.return (`Error "Sync already in progress")
    else (
      t.set_sync_in_progress true;
      Lwt.finalize aux (fun () -> t.set_sync_in_progress false; Lwt.return ())
    )

  let sync_in_progress t = t.sync_in_progress
end
