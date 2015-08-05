(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

(* Never used, but needed to create the store. *)
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)

module Bytes = String

module Main (Stack:STACKV4) (Conf:KV_RO) (Clock:V1.CLOCK) = struct
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
    str_digest

  let hash token = token
    |> Cstruct.of_string
    |> Nocrypto.Hash.digest `SHA256
    |> to_hex

  (* We store the hash rather than the original to avoid storing the secret in the server binary. *)
  let user_of_token token =
    let hashed = hash token in
    let device = Devices.lookup hashed in
    if device = None then Log.warn "Invalid access token used (hash = %S)" hashed;
    device

  let handle_request s conn_id request body =
    (* Instead of handing the Git store directly to [Server.handle_request], wrap it in a function
     * that authenticates the request first, so it can't use it without checking. *)
    let get_db reason =
      let uri = Cohttp.Request.uri request in
      match Uri.get_query_param uri "token" with
      | None -> `Error (S.respond_error ~status:`Bad_request ~body:"Missing access token" ())
      | Some token ->
          match user_of_token token with
          | None -> `Error (S.respond_error ~status:`Unauthorized ~body:"Invalid access token" ())
          | Some user ->
              Log.info "Handling request for %S" user;
              `Ok (s reason) in
    Server.handle_request get_db conn_id request body

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
