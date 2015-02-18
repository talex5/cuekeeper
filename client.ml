open Lwt

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)
module M = Ck_model.Make(Store)
module T = Ck_template.Make(M)

let start (main:#Dom.node Js.t) =
  let config = Irmin_mem.config () in
  let task s = Irmin.Task.create ~date:0L ~owner:"User" s in
  Store.create config task >>=
  M.make >>= fun m ->
  let root = React.S.value (M.root m) in
  M.add_area ~parent:root ~name:"Personal" ~description:"" m >>= fun personal ->
  M.add_area ~parent:personal ~name:"House" ~description:"" m >>= fun _house ->
  M.add_area ~parent:personal ~name:"Car" ~description:"" m >>= fun _car ->
  M.add_area ~parent:root ~name:"Work" ~description:"" m >>= fun work ->
  M.add_project ~parent:work ~name:"Make CueKeeper" ~description:"" m >>= fun ck ->
  M.add_action ~parent:ck ~name:"Switch to TyXML" ~description:"" m >>= fun _ty ->
  let body =
    T.make_tree m in
  main##appendChild (Tyxml_js.To_dom.of_node body) |> ignore;
  Lwt_js_events.async (fun () -> Lwt_js_events.click main >>= fun _ev ->
    M.set_name m work "Working" >>= fun () ->
    M.add_project ~parent:work ~name:"Adding events" ~description:"" m >|= fun _ ->
    print_endline "Updated name!"
  );
  return ()

let () =
  match Dom_html.tagged (Dom_html.getElementById "ck_tree") with
  | Dom_html.Div d -> Lwt_js_events.async (fun () -> start d)
  | _ -> failwith "Bad tree element"
