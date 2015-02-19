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

module Make (M : Ck_sigs.MODEL) = struct

  (*
  let mint =
    let i = ref 0 in
    fun () ->
      incr i;
      string_of_int !i

  let add node =
    let key = M.name node in
    let drop = "drop-" ^ mint () in
    <:html<
      <a class='add' data-dropdown=$str:drop$ aria-controls=$str:drop$ aria-expanded="false">+action</a>
      <div id=$str:drop$ data-dropdown-content="" class="f-dropdown content" aria-hidden="true" aria-autoclose="false" tabindex="-1">
        <form>
          <label>New action:</label>
          <input type="hidden" value=$str:key$/>
          <div class="row">
            <div class="small-12 columns">
              <div class="row collapse">
                <div class="small-9 columns">
                  <input type="text" placeholder="Name"/>
                </div>
                <div class="small-3 columns">
                  <a href="#" class="button postfix">Add</a>
                </div>
              </div>
            </div>
          </div>
        </form>
      </div>
    >>
  *)

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

  let render_new_action m = <:html<
    <form method="POST" action="/add">
      <fieldset>
      <legend><input type="text" name="name" placeholder="Name"/></legend>

      <div class="row">
        <div class="large-12 columns">
          <select name="parent">
            $list:all_areas_and_projects m$
          </select>
        </div>
      </div>
      <div class="row">
        <div class="large-12 columns">
          <input type="radio" name="type" value="Area"><label>Area</label></input>
          <input type="radio" name="type" value="Project"><label>Project</label></input>
          <input type="radio" name="type" value="Action" checked=""><label>Action</label></input>
        </div>
      </div>
      <div class="row">
        <div class="large-12 columns">
          <textarea name="description" placeholder="Description" rows='6'/>
        </div>
      </div>
      <div class="row">
        <div class="large-12 columns">
          <input class="right" type="submit" value="Add"/>
        </div>
      </div>
      </fieldset>
    </form>
  >>

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

  let make_details_panel ~remove item =
    let open Html5 in
    let closed, set_closed = React.S.create ["ck-details"] in
    let close () =
      let open Lwt in
      set_closed ["ck-details"; "closed"];       (* Start fade-out animation *)
      Lwt.async (fun () -> Lwt_js.sleep 0.5 >|= remove)   (* Actually remove *)
    in
    let panel =
      div ~a:[R.Html5.a_class closed] [
        div ~a:[a_class ["panel"]] [
          h4 [R.Html5.pcdata item.M.details_name];
          a ~a:[a_onclick (fun _ -> close (); true)] [pcdata "(close)"];
        ]
      ] in
    (panel, close)

  let make_details_area m =
    let details_pane, details_handle = ReactiveData.RList.make [] in
    let show_node uuid =
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
          ReactiveData.RList.insert (uuid, make_details_panel ~remove details) (List.length current_items) details_handle;
      | Some (_id, (_panel, close)) -> close ()
      in
    (ReactiveData.RList.map (fun (_uuid, (panel, _close)) -> panel) details_pane, show_node)

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
