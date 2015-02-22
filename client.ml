open Lwt

(* let () = Log.(set_log_level DEBUG) *)

module Store = Irmin.Basic(Html_storage.Make)(Irmin.Contents.String)
module M = Ck_model.Make(Store)
module T = Ck_template.Make(M)

let start (main:#Dom.node Js.t) =
  let config = Html_storage.config "CueKeeper" in
  let task s =
    let date = Unix.time () |> Int64.of_float in
    Irmin.Task.create ~date ~owner:"User" s in
  Store.create config task >>=
  M.make >>= fun m ->
(*
  let root = React.S.value (M.root m) in
  M.add_area ~parent:root ~name:"Personal" ~description:"" m >>= fun personal ->
  M.add_area ~parent:personal ~name:"House" ~description:"" m >>= fun _house ->
  M.add_area ~parent:personal ~name:"Car" ~description:"" m >>= fun _car ->
  M.add_area ~parent:root ~name:"Work" ~description:"" m >>= fun work ->
  M.add_project ~parent:work ~name:"Make CueKeeper" ~description:"" m >>= fun ck ->
  M.add_action ~parent:ck ~name:"Switch to TyXML" ~description:"" m >>= fun _ty ->
*)
  T.make_top m
  |> List.iter (fun child -> main##appendChild (Tyxml_js.To_dom.of_node child) |> ignore);
  return ()

let () =
  match Dom_html.tagged (Dom_html.getElementById "ck_main") with
  | Dom_html.Div d -> Lwt_js_events.async (fun () -> start d)
  | _ -> failwith "Bad tree element"
