(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

(* Never used, but needed to create the store. *)
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)

module Bundle = Tc.Pair(Store.Private.Slice)(Store.Head)

let show_head = function
  | None -> "(none)"
  | Some head -> String.sub (Irmin.Hash.SHA1.to_hum head) 0 6

module Main (Stack:STACKV4) (Conf:KV_RO) (Clock:V1.CLOCK) = struct
  module TCP  = Stack.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (Conf) (Clock)
  module S = Cohttp_mirage.Server(TLS)

  let respond_static segments =
    let path = String.concat "/" segments in
    match Static.read path with
    | Some body -> S.respond_string ~status:`OK ~body ()
    | None -> S.respond_not_found ()

  (* Split a URI into a list of path segments *)
  let split_path uri =
    let path = Uri.path uri in
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "")
      (aux (Re_str.(split_delim (regexp_string "/") path)))

  (* Import bundle from client into store. *)
  let accept_push s body =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    let s = s "import" in
    Cohttp_lwt_body.to_string body >>= fun body ->
    let buf = Mstruct.of_string (B64.decode body) in
    let (slice, head) = Bundle.read buf in
    Store.head s >>= function
    | Some server_head when server_head = head ->
        S.respond_string ~headers ~status:`OK ~body:"ok" ()
    | server_head ->
    Store.import s slice >>= fun () ->
    let commit_store = Store.Private.commit_t s in
    Store.Private.Commit.mem commit_store head >>= function
    | false ->
        let msg = "New head not found after import!" in
        Log.warn "%s" msg;
        S.respond_string ~headers ~status:`Bad_request ~body:msg ()
    | true ->
    Store.fast_forward_head s head >>= function
    | false ->
        let msg = Printf.sprintf "Non-fast-forward push attempted: %s -> %s"
          (show_head server_head) (show_head (Some head)) in
        Log.warn "%s" msg;
        S.respond_string ~headers ~status:`OK ~body:"not-fast-forward" ()
    | true ->
        Log.info "Update master %s -> %s" (show_head server_head) (show_head (Some head));
        S.respond_string ~headers ~status:`OK ~body:"ok" ()

  (* Export changes in the store since [last_known] to a bundle for the client. *)
  let fetch s last_known =
    let headers = Cohttp.Header.init_with "Content-Type" "application/octet-stream" in
    let s = s "export" in
    Store.head s >>= function
    | None -> S.respond_string ~headers ~status:`OK ~body:"" ()
    | Some head ->
    let basis =
      match last_known with
      | None -> []
      | Some c -> [Irmin.Hash.SHA1.of_hum c] in
    Store.export s ~min:basis ~max:[head] >>= fun slice ->
    let bundle = (slice, head) in
    let buf = Cstruct.create (Bundle.size_of bundle) in
    let rest = Bundle.write bundle buf in
    assert (Cstruct.len rest = 0);
    let body = Cstruct.to_string buf |> B64.encode in
    S.respond_string ~headers ~status:`OK ~body ()

  let handle_request s _conn_id request body =
    Lwt.catch (fun () ->
      match Cohttp.Request.meth request, split_path (Cohttp.Request.uri request) with
      | `GET, ["fetch"] -> fetch s None
      | `GET, ["fetch"; last_known] -> fetch s (Some last_known)
      | `POST, ["push"] -> accept_push s body
      | `GET, ([] | [""]) -> respond_static ["index.html"]
      | `GET, segments -> respond_static segments
      | _ -> S.respond_error ~status:`Method_not_allowed ~body:"Invalid request" ()
    ) (fun ex ->
      Log.warn "Unhandled exception processing HTTP request: %s" (Printexc.to_string ex);
      fail ex
    )

  let start stack conf _clock =
    Store.create (Irmin_mem.config ()) task >>= fun s ->
    let http = S.make ~conn_closed:ignore ~callback:(handle_request s) () in
    X509.certificate conf `Default >>= fun cert ->
    let tls_config = Tls.Config.server ~certificates:(`Single cert) () in
    Stack.listen_tcpv4 stack ~port:8443 (fun flow ->
      let peer, port = TCP.get_dest flow in
      Log.info "Connection from %s (client port %d)" (Ipaddr.V4.to_string peer) port;
      TLS.server_of_flow tls_config flow >>= function
      | `Error _ -> Log.warn "TLS failed"; TCP.close flow
      | `Eof     -> Log.warn "TLS eof"; TCP.close flow
      | `Ok flow  ->
      S.listen http flow >>= fun () ->
      TLS.close flow
    );
    Stack.listen stack

end
