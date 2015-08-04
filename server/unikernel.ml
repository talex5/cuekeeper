(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

(* Never used, but needed to create the store. *)
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)

module Main (Stack:STACKV4) (Conf:KV_RO) (Clock:V1.CLOCK) = struct
  module TCP  = Stack.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (Conf) (Clock)
  module S = Cohttp_mirage.Server(TLS)
  module Server = Server.Make(Store)(S)

  let start stack conf _clock =
    Store.create (Irmin_mem.config ()) task >>= fun s ->
    let http = S.make ~conn_closed:ignore ~callback:(Server.handle_request s) () in
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
