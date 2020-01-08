(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

let src = Logs.Src.create "unikernel" ~doc:"Main unikernel code"
module Log = (val Logs.src_log src : Logs.LOG)

module IO = struct
  (* We don't use Git's sync, but it requires us to provide this module anyway. *)
  type ic
  type oc
  type ctx
  let with_connection ?ctx:_ _uri ?init:_ _f = failwith "with_connection"
  let read_all _ = failwith "read_all"
  let read_exactly _ _ = failwith "read_exactly"
  let write _ _ = failwith "write"
  let flush _ = failwith "flush"
  let ctx () = Lwt.return_none
end

module Store = Irmin_git.Mem.Make(IO)(Git.Inflate.None)
    (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)

module Main
    (Stack:Mirage_stack.V4)
    (Conf:Mirage_kv.RO)
    (Clock:Mirage_clock.PCLOCK) = struct
  module TCP  = Stack.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (Conf) (Clock)
  module S = Cohttp_mirage.Server(TLS)
  module Server = Server.Make(Store)(S)

  let hex_chars = "0123456789abcdef"

  let to_hex raw_digest =
    let str_digest = Bytes.create (Cstruct.len raw_digest * 2) in
    for i = 0 to Cstruct.len raw_digest - 1 do
      Bytes.set str_digest (i * 2) hex_chars.[(Cstruct.get_uint8 raw_digest i land 0xf0) lsr 4];
      Bytes.set str_digest (i * 2 + 1) hex_chars.[Cstruct.get_uint8 raw_digest i land 0xf];
    done;
    Bytes.unsafe_to_string str_digest

  let hash token =
    Cstruct.of_string token
    |> Nocrypto.Hash.digest `SHA256
    |> to_hex

  (* We store the hash rather than the original to avoid storing the secret in the server binary. *)
  let user_of_token token =
    let hashed = hash token in
    let device = Devices.lookup hashed in
    if device = None then Log.warn (fun f -> f "Invalid access token used (hash = %S)" hashed);
    device

  let handle_request s conn_id request body =
    (* Instead of handing the Git store directly to [Server.handle_request], wrap it in a function
     * that authenticates the request first, so it can't use it without checking. *)
    let get_db () =
      let uri = Cohttp.Request.uri request in
      match Uri.get_query_param uri "token" with
      | None -> `Error (S.respond_error ~status:`Bad_request ~body:"Missing access token" ())
      | Some token ->
          match user_of_token token with
          | None -> `Error (S.respond_error ~status:`Unauthorized ~body:"Invalid access token" ())
          | Some user ->
              Log.info (fun f -> f "Handling request for %S" user);
              `Ok s in
    Server.handle_request get_db conn_id request body

  let start stack conf _clock () =
    Store.Repo.v (Irmin_mem.config ()) >>= Store.master >>= fun s ->
    let http = S.make ~conn_closed:ignore ~callback:(handle_request s) () in
    X509.certificate conf `Default >>= fun cert ->
    let tls_config = Tls.Config.server ~certificates:(`Single cert) () in
    Stack.listen_tcpv4 stack ~port:8443 (fun flow ->
      let peer, port = TCP.dst flow in
      Log.info (fun f -> f "Connection from %a (client port %d)" Ipaddr.V4.pp peer port);
      TLS.server_of_flow tls_config flow >>= function
      | Error `Closed -> Log.warn (fun f -> f "TLS eof"); TCP.close flow
      | Error e -> Log.warn (fun f -> f "TLS failed: %a" TLS.pp_write_error e); TCP.close flow
      | Ok flow  ->
      S.listen http flow >>= fun () ->
      TLS.close flow
    );
    Stack.listen stack
end
