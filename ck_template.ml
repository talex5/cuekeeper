(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js

let maybe_ul = function
  | [] -> Html5.pcdata ""
  | items -> Html5.ul items

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

  let make_action node =
    let open Html5 in
    li ~a:[a_class ["action"]] [
      pcdata (M.name node);
    ]

  let rec make_project node =
    let open Html5 in
    li ~a:[a_class ["project"]] [
      pcdata (M.name node);
      maybe_ul (List.concat [
        M.actions node  |> List.map make_action;
        M.projects node |> List.map make_project;
      ])
    ]

  let rec make_area node : _ Html5.elt =
    let open Html5 in
    li ~a:[a_class ["area"]] [
      pcdata (M.name node);
      maybe_ul (List.concat [
        M.actions node  |> List.map make_action;
        M.projects node |> List.map make_project;
        M.areas node    |> List.map make_area;
      ])
    ]

  let all_areas_and_projects m =
    M.all_areas_and_projects m
    |> List.map (fun node ->
      let path = M.full_name node in
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

  let make_tree m =
    let open Html5 in
    let root = M.root m in
    ul [make_area root]

end
