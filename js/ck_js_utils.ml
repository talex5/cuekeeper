(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let ignore_listener = ignore

let inside elem child =
  let elem = (elem :> Dom.node Js.t) in
  let rec aux child =
    if elem == child then true
    else (
      Js.Opt.case (child##parentNode)
        (fun () -> false)
        aux
    ) in
  aux (child :> Dom.node Js.t)

let keycode_escape = 27

let pos_from_root (elem : #Dom_html.element Js.t) =
  let rec aux x y elem =
    let x = x + elem##offsetLeft in
    let y = y + elem##offsetTop in
    Js.Opt.case (elem##offsetParent)
      (fun () -> (x, y))
      (fun parent -> aux x y parent) in
  aux 0 0 (elem :> Dom_html.element Js.t)

let async ~name (fn:unit -> unit Lwt.t) =
  Lwt_js_events.async (fun () ->
    Lwt.catch fn (fun ex ->
      Printf.printf "Async error in '%s'" name;
      Lwt.fail ex
    )
  )

let auto_focus input =
  async ~name:"focus" (fun () ->
    let elem = Tyxml_js.To_dom.of_input input in
    elem##select ();
    Lwt.return ()
  )
