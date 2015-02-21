(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js

(* Get the index of an item in an assoc list. *)
let index_of key items =
  let rec aux i = function
    | [] -> None
    | (k, _v) :: _ when k = key -> Some i
    | _ :: xs -> aux (i + 1) xs in
  aux 0 items

let (>>?=) = Js.Opt.bind

module Make (M : Ck_sigs.MODEL) = struct

  let current_highlight, set_highlight = React.S.create None

  let class_of_node_type = function
    | `Area -> ["area"]
    | `Project -> ["project"]
    | `Action -> ["action"]

  let rec make_node_view ~show_node (node:M.node_view) : _ Html5.elt =
    let open Html5 in
    let children = node.M.child_views |> ReactiveData.RList.map (make_node_view ~show_node) in
    let clicked _ev = show_node node.M.uuid; true in
    li ~a:[R.Html5.a_class (React.S.map class_of_node_type node.M.node_type)] [
      Html5.a ~a:[a_href "#"; a_onclick clicked] [R.Html5.pcdata node.M.name];
      R.Html5.ul children;
    ]

  let make_work_view ~show_node actions =
    let children = actions |> ReactiveData.RList.map (make_node_view ~show_node) in
    let open Html5 in [
      h4 [pcdata "Actions"];
      R.Html5.ul children;
    ]

  let all_areas_and_projects m =
    M.all_areas_and_projects m
    |> List.map (fun (path, node) ->
      let uuid = M.uuid node in
      <:html< <option value=$str:uuid$>$str:path$</option>&>>
    )

  let make_tree ~show_node current_mode m =
    let open Html5 in
    let tab mode contents =
      let cl = current_mode |> React.S.map (fun m ->
        if m = mode then ["content"; "active"] else ["content"]
      ) in
      div ~a:[R.Html5.a_class cl] contents in
    let process = M.process_tree m |> make_node_view ~show_node in
    let work = M.work_tree m |> make_work_view ~show_node in
    div ~a:[a_class ["tabs-content"]] [
      tab `Process [ul [process]];
      tab `Work work;
    ]

  let make_mode_switcher current_mode set_current_mode =
    let open Html5 in
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
    ]

  let make_child_adder m item =
    let open Html5 in
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
      React.S.bind editing (function
        (* When we're not editing, display the add buttons. *)
        | None ->
            item.M.details_type |> React.S.map (function
              | `Action -> []
              | `Project -> [
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
                let name = List.assoc "name" f in
                let adder =
                  match node_type with
                  | `Action -> M.add_action
                  | `Project -> M.add_project
                  | `Area -> M.add_area in
                Lwt_js_events.async (fun () -> adder m ~parent:item.M.details_uuid ~name ~description:"");
                set_editing None;
              );
              true in
            React.S.const [
              form ~a:[a_onsubmit do_add] [
                input ~a:[a_name "name"; a_placeholder "Name"] ()];
                input ~a:[a_input_type `Submit; a_value "Add"; a_onclick do_add] ();
                a ~a:[a_onclick (fun _ev -> set_editing None; true)] [pcdata " (cancel)"];
              ]
      )
    in
    let changes = React.S.changes widgets |> React.E.map (fun x -> ReactiveData.RList.Set x) in
    let rlist = ReactiveData.RList.make_from (React.S.value widgets) changes in
    ul [
      R.Html5.li ~a:[a_class ["add"]] rlist
    ]

  let make_details_panel m ~show_node ~remove ~uuid item =
    let open Html5 in
    let closed, set_closed = React.S.create false in
    let close () =
      let open Lwt in
      set_closed true;                          (* Start fade-out animation *)
      Lwt.async (fun () -> Lwt_js.sleep 0.5 >|= remove)  (* Actually remove *)
    in
    let cl =
      React.S.bind closed (fun closed ->
        current_highlight |> React.S.map (fun highlight ->
          "ck-details" :: List.concat [
            if highlight = Some uuid then ["ck-highlight"] else [];
            if closed then ["closed"] else [];
          ]
        )
      ) in
    let children = item.M.details_children |> ReactiveData.RList.map (make_node_view ~show_node) in
    div ~a:[R.Html5.a_class cl] [
      div ~a:[a_class ["panel"]] [
        a ~a:[a_onclick (fun _ -> close (); true); a_class ["close"]] [entity "#215"];
        h4 [R.Html5.pcdata item.M.details_name];
        R.Html5.ul children;
        make_child_adder m item;
        div ~a:[a_class ["description"]] [
          p [R.Html5.pcdata item.M.details_description];
        ]
      ]
    ]

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
    let open Html5 in
    let current_mode, set_current_mode = React.S.create `Process in
    let details_area, show_node = make_details_area m in
    [
      make_mode_switcher current_mode set_current_mode;
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["medium-6"; "columns"]] [
          make_tree ~show_node current_mode m;
        ];
        R.Html5.div ~a:[a_class ["medium-6"; "columns"]] (
          details_area;
        );
      ]
    ]
end
