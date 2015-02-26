(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils

(* (forces return type to be unit) *)
let async : (unit -> unit Lwt.t) -> unit = Lwt_js_events.async

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items

let (>>?=) = Js.Opt.bind

module Make (M : Ck_sigs.MODEL) = struct
  module View = M.View

  let current_highlight, set_highlight = React.S.create None

  let with_done cls = function
    | `Project {Ck_sigs.pstate = `Done}
    | `Action {Ck_sigs.astate = `Done} -> "ck-done" :: cls
    | _ -> cls

  let class_of_node_type = function
    | `Area -> "ck-area"
    | `Project _ -> "ck-project"
    | `Action _ -> "ck-action"
    | `Deleted -> "ck-deleted"

  let class_of_time_and_type ctime node_type =
    let ty = with_done ["ck-item"; class_of_node_type node_type] node_type in
    let lifetime = Unix.gettimeofday () -. ctime in
    if lifetime >= 0.0 && lifetime <  1.0 then "new" :: ty
    else ty

  let toggle_label ~set_details ~current details =
    let l =
      match details with
      | `Done -> "✓"
      | `Next -> "n"
      | `Waiting -> "w"
      | `Future -> "f"
      | `Active -> "a"
      | `SomedayMaybe -> "sm" in
    let cl = if current = details then "ck-active-" ^ l else "ck-inactive" in
    let changed _ev =
      if current <> details then set_details details; true in
    a ~a:[a_class [cl]; a_onclick changed] [pcdata l]

  let make_toggles ~set_details current options =
    options |> List.map (toggle_label ~set_details ~current)

  let toggles_for_type m node = function
    | `Action {Ck_sigs.astate = s; _} ->
        let set_details n =
          Lwt_js_events.async (fun () ->
            M.set_details m node.View.uuid (`Action {Ck_sigs.astate = n})
          ) in
        make_toggles ~set_details s [`Done; `Next; `Waiting; `Future]
    | `Project {Ck_sigs.pstate = s; _} ->
        let set_details n =
          Lwt_js_events.async (fun () ->
            M.set_details m node.View.uuid (`Project {Ck_sigs.pstate = n})
          ) in
        make_toggles ~set_details s [`Done; `Active; `SomedayMaybe]
    | `Area | `Deleted -> []

  let make_state_toggles m node =
    let init = node.View.init_node_type |> toggles_for_type m node in
    let toggles = node.View.node_type >|~= toggles_for_type m node in
    rlist_of ~init toggles

  (* Fade item in and out based on state. *)
  let animated node item_ref =
    let cancel = ref ignore in
    node.View.state >|~= fun state ->
      !cancel ();
      cancel := ignore;
      match state with
      | `New -> ["new"]
      | `Moved full_height ->
          cancel := Ck_animate.fade_in_move ~full_height item_ref;
          ["moved"]
      | `Current -> []
      | `Removed (full_height, _time) ->
          begin match !item_ref with
          | None -> ()
          | Some elem ->
              let elem = Tyxml_js.To_dom.of_element elem in
              full_height := Some elem##offsetHeight;
              cancel := Ck_animate.fade_out elem
          end;
          ["removed"]

  (* A <li>[toggles] name x [children]</li> element *)
  let rec make_node_view m ~show_node node : _ Html5.elt =
    let children = node.View.child_views
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    let clicked _ev = show_node node.View.uuid; true in
    let delete _ev = async (fun () -> M.delete m node.View.uuid); true in
    let item_cl = React.S.map (class_of_time_and_type node.View.ctime) node.View.node_type in
    let li_ref = ref None in
    let li_state = animated node li_ref in
    let result =
      li ~a:[R.Html5.a_class li_state] [
        span ~a:[R.Html5.a_class item_cl] [
          R.Html5.span ~a:[a_class ["ck-toggles"]] (make_state_toggles m node);
          span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
            a ~a:[a_class ["ck-title"]; a_href "#"; a_onclick clicked] [R.Html5.pcdata node.View.name];
          ];
          if M.is_root node.View.uuid then pcdata ""
          else a ~a:[a_class ["delete"]; a_onclick delete] [entity "cross"];
        ];
        R.Html5.ul children;
      ] in
    li_ref := Some result;
    result

  let make_work_view m ~show_node actions =
    let children = actions
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    [
      h4 [pcdata "Next actions"];
      R.Html5.ul children;
    ]

  let make_sync history =
    let items =
      rlist_of ~init:(React.S.value history) history
      |> ReactiveData.RList.map (fun (date, summary) ->
          let open Unix in
          let tm = gmtime date in
          let msg = Printf.sprintf "%04d-%02d-%02d: %s"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            summary in
          li [pcdata msg]
      ) in
    [
      h4 [pcdata "Recent changes"];
      R.Html5.ol ~a:[a_class ["ck-history"]] items;
    ]

  let make_tree ~show_node current_mode m =
    let tab mode contents =
      let cl = current_mode |> React.S.map (fun m ->
        if m = mode then ["content"; "active"] else ["content"]
      ) in
      div ~a:[R.Html5.a_class cl] contents in
    let process_tree = M.process_tree m  in
    let process = make_node_view m ~show_node process_tree in
    let work = M.work_tree m |> make_work_view m ~show_node in
    div ~a:[a_class ["tabs-content"]] [
      tab `Process [ul [process]];
      tab `Work work;
      tab `Sync (make_sync (M.history m));
    ]

  let make_mode_switcher current_mode set_current_mode =
    let item name mode =
      let cl = current_mode |> React.S.map (fun m ->
        if m = mode then ["active"] else []
      ) in
      let clicked _ev = set_current_mode mode; true in
      let button = a ~a:[a_href "#"; a_onclick clicked] [pcdata name] in
      dd ~a:[R.Html5.a_class cl] [button] in

    dl ~a:[a_class ["sub-nav"]] [
      item "Process" `Process;
      item "Work" `Work;
      item "Review" `Review;
      item "Contact" `Contact;
      item "Schedule" `Schedule;
      item "Sync" `Sync;
    ]

  let make_child_adder m item =
    let editing, set_editing = React.S.create None in
    let add_button ntype label =
      let start_editing (ev:#Dom_html.event Js.t) =
        (* Find the parent <li> before we remove the button element. *)
        let parent =
          ev##target >>?= fun button ->
          button##parentNode >>?= Dom_html.CoerceTo.element in
        (* Replace the buttons with a form. *)
        set_editing (Some ntype);
        (* Now find the new form input and focus it. *)
        let input =
          parent >>?= fun parent ->
          parent##querySelector (Js.string "input") >>?= Dom_html.CoerceTo.input in
        Js.Opt.iter input (fun i -> i##focus ());
        true in
      a ~a:[a_onclick start_editing] [pcdata label] in
    let widgets =
      editing >>~= (function
        (* When we're not editing, display the add buttons. *)
        | None ->
            item.View.node_type >|~= (function
              | `Deleted -> []
              | `Action _ -> []
              | `Project _ -> [
                  add_button `Action "+action";
                  add_button `Project "+sub-project";
                ]
              | `Area -> [
                  add_button `Project "+project";
                  add_button `Action "+action";
                  add_button `Area "+sub-area";
                ]
            )
        (* When we are editing, display the form. *)
        | Some node_type ->
            let do_add ev =
              let form =
                ev##target >>?= fun target ->
                target##parentNode >>?= fun parent ->
                Dom_html.CoerceTo.element parent >>?= fun parent ->
                parent##querySelector (Js.string "form") >>?=
                Dom_html.CoerceTo.form in
              Js.Opt.iter form (fun form ->
                let f = Form.get_form_contents form in
                let name = List.assoc "name" f |> String.trim in
                if name <> "" then (
                  let adder =
                    match node_type with
                    | `Action -> M.add_action
                    | `Project -> M.add_project
                    | `Area -> M.add_area in
                  Lwt_js_events.async (fun () -> adder m ~parent:item.View.uuid ~name ~description:"")
                );
                set_editing None;
              );
              true in
            React.S.const [
              form ~a:[a_onsubmit do_add] [
                input ~a:[a_name "name"; a_placeholder "Name"] ();
                input ~a:[a_input_type `Submit; a_value "Add"; a_onclick do_add] ();
                a ~a:[a_onclick (fun _ev -> set_editing None; true)] [pcdata " (cancel)"];
              ]
            ]
      )
    in
    let rlist = rlist_of ~init:(React.S.value widgets) widgets in
    ul [
      R.Html5.li ~a:[a_class ["add"]] rlist
    ]

  let make_editable_title m node =
    let editing, set_editing = React.S.create false in
    let widgets =
      editing >|~= (function
        | false ->
            let edit _ev = set_editing true; true in [
              span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
                a ~a:[a_class ["ck-title"]; a_onclick edit] [R.Html5.pcdata node.View.name];
              ]
            ]
        | true ->
            let submit ev =
              let form = ev##target >>?= Dom_html.CoerceTo.form in
              Js.Opt.iter form (fun form ->
                let f = Form.get_form_contents form in
                let name = List.assoc "name" f |> String.trim in
                if name <> "" then (
                  async (fun () -> M.set_name m node.View.uuid name)
                )
              );
              set_editing false;
              true in
            let old_name = React.S.value node.View.name in [
              form ~a:[a_class ["rename"]; a_onsubmit submit] [
                input ~a:[a_name "name"; a_placeholder "Name"; a_size 25; a_value old_name] ();
                input ~a:[a_input_type `Submit; a_value "OK"] ();
              ]
            ]
      ) in
    rlist_of ~init:(React.S.value widgets) widgets

  let make_details_panel m ~show_node ~remove ~uuid item =
    let closed, set_closed = React.S.create false in
    let close () =
      let open Lwt in
      set_closed true;                          (* Start fade-out animation *)
      Lwt.async (fun () -> Lwt_js.sleep 0.5 >|= remove)  (* Actually remove *)
    in
    let elem = ref None in
    let cl =
      let cancel_close = ref ignore in
      closed >>~= (fun closed ->
        !cancel_close ();
        begin match closed, !elem with
        | true, Some elem ->
            let elem = Tyxml_js.To_dom.of_element elem in
            cancel_close := Ck_animate.fade_out elem
        | _ -> () end;
        current_highlight |> React.S.map (fun highlight ->
          "ck-details" :: List.concat [
            if highlight = Some uuid then ["ck-highlight"] else [];
            if closed then ["closed"] else [];
          ]
        )
      ) in
    let title_cl =
      item.View.node_type >|~= (fun node_type -> with_done ["ck-heading"; class_of_node_type node_type] node_type) in
    let children = item.View.child_views
      |> ReactiveData.RList.map (make_node_view m ~show_node) in
    let result =
      div ~a:[R.Html5.a_class cl] [
        a ~a:[a_onclick (fun _ -> close (); true); a_class ["close"]] [entity "#215"];
        div ~a:[R.Html5.a_class title_cl] [
          R.Html5.span ~a:[a_class ["ck-toggles"]] (make_state_toggles m item);
          R.Html5.div ~a:[a_class ["inline"]] (make_editable_title m item);
        ];
        R.Html5.ul children;
        make_child_adder m item;
        div ~a:[a_class ["description"]] [
          p [R.Html5.pcdata item.View.description];
        ]
      ] in
    elem := Some result;
    result

  let make_details_area m =
    let details_pane, details_handle = ReactiveData.RList.make [] in
    let rec show_node uuid =
      let remove () =
        let current_items = ReactiveData.RList.value details_pane in
        match index_of uuid current_items with
        | None -> ()
        | Some i-> ReactiveData.RList.remove i details_handle in
      let current_items = ReactiveData.RList.value details_pane in
      let existing =
        try
          Some (List.find (fun (id, _) -> id = uuid) current_items)
        with Not_found -> None in
      match existing with
      | None ->
          let details = M.details m uuid in
          ReactiveData.RList.insert (uuid, make_details_panel m ~show_node ~remove ~uuid details) (List.length current_items) details_handle;
      | Some _ ->
          let open Lwt in
          set_highlight (Some uuid);
          Lwt.async (fun () ->
            Lwt_js.sleep 1.0 >|= fun () ->
            if React.S.value current_highlight = Some uuid then set_highlight None
          )
      in
    (ReactiveData.RList.map snd details_pane, show_node)

  let make_top m =
    let current_mode, set_current_mode = React.S.create `Process in
    let details_area, show_node = make_details_area m in
    [
      make_mode_switcher current_mode set_current_mode;
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["medium-6"; "columns"; "ck-tree"]] [
          make_tree ~show_node current_mode m;
        ];
        R.Html5.div ~a:[a_class ["medium-6"; "columns"]] (
          details_area;
        );
      ]
    ]
end
