(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

module Make(Clock : Ck_clock.S) = struct
  type listener = (ic:Lwt_io.input_channel -> oc:Lwt_io.output_channel -> unit)
  let ignore_listener : listener = fun ~ic:_ ~oc:_ -> ()
  let listener = ref ignore_listener

  module Test_network = struct
    module IO = struct
      include Lwt

      type ic = Lwt_io.input_channel
      type oc = Lwt_io.output_channel
      type conn = unit

      let iter = Lwt_list.iter_s
      let flush = Lwt_io.flush

      let read ic n = Lwt_io.read ~count:n ic
      let read_line ic =
        Lwt.catch (fun () -> Lwt_io.read_line ic >|= fun line -> Some line)
          (function
            | End_of_file -> return None
            | ex -> raise ex
          )
      let write = Lwt_io.write
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
      Log.info "connect(%s)" (Uri.to_string uri);
      let ic_req, oc_req = Lwt_io.pipe () in
      let ic_resp, oc_resp = Lwt_io.pipe () in
      !listener ~ic:ic_req ~oc:oc_resp;
      return ((), ic_resp, oc_req)
  end

  module Server = Cohttp_lwt.Make_server(Test_network.IO)
  module Client = Cohttp_lwt.Make_client(Test_network.IO)(Test_network)
end
