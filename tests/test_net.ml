(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt.Infix

let client = Logs.Src.create "client-net" ~doc:"CueKeeper test client"
module Client_log = (val Logs.src_log client : Logs.LOG)

let server = Logs.Src.create "server-net" ~doc:"CueKeeper test server"
module Server_log = (val Logs.src_log server : Logs.LOG)

module Make(Clock : Ck_clock.S) = struct
  type listener = (ic:Lwt_io.input_channel -> oc:Lwt_io.output_channel -> unit)
  let ignore_listener : listener = fun ~ic:_ ~oc:_ -> ()
  let listener = ref ignore_listener

  module Test_network(Log : Logs.LOG) = struct
    type error = unit
    let pp_error f () = Format.pp_print_string f "error"

    module IO = struct
      include Lwt

      type error = unit
      let pp_error f () = Format.pp_print_string f "IO error"

      type ic = Lwt_io.input_channel
      type oc = Lwt_io.output_channel
      type conn = unit

      let iter = Lwt_list.iter_s
      let flush = Lwt_io.flush

      let read ic n =
        Lwt_io.read ~count:n ic >|= fun d ->
        Log.debug (fun f -> f "<<< %S" d);
        d

      let read_line ic =
        Lwt.catch
          (fun () -> Lwt_io.read_line ic >|= fun line ->
            Log.info (fun f -> f "<<< %S" line);
            Some line
          )
          (function
            | End_of_file -> Lwt.return None
            | ex -> raise ex
          )
      let write = Lwt_io.write

      let catch f = f () >|= fun x -> Ok x
    end

    type ctx = unit
    
    let sexp_of_ctx _ = failwith "sexp_of_ctx"
    let default_ctx = ()
    let close_in ch = Lwt.async (fun () -> Lwt_io.close ch)
    let close_out ch = Lwt.async (fun () -> Lwt_io.close ch)
    let close ic oc =
      close_in ic;
      close_out oc

    let connect_uri ~ctx:_ uri =
      Log.info (fun f -> f "connect(%a)" Uri.pp uri);
      let ic_req, oc_req = Lwt_io.pipe () in
      let ic_resp, oc_resp = Lwt_io.pipe () in
      !listener ~ic:ic_req ~oc:oc_resp;
      Lwt.return ((), ic_resp, oc_req)
  end

  module CNet = Test_network(Client_log)
  module SNet = Test_network(Server_log)

  module Server = Cohttp_lwt.Make_server(SNet.IO)
  module Client = struct
    module C = Cohttp_lwt.Make_client(CNet.IO)(CNet)
    let get ?headers uri = C.get ?headers uri >|= fun r -> `Ok r
    let post ?body ?headers uri = C.post ?body ?headers uri >|= fun r -> `Ok r
  end
end
