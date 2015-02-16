(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let ul = function
  | [] -> []
  | items ->
      <:html<
        <ul>
          $list:items$
        </ul>
      >>

let mint =
  let i = ref 0 in
  fun () ->
    incr i;
    string_of_int !i

let add node =
  let key = Ck_irmin_model.name node in
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

let render_action node = <:html<
  <li class='action'>$str:Ck_irmin_model.name node$</li>
>>

let rec render_project node = <:html<
  <li class='project'>$str:Ck_irmin_model.name node$ $add node$</li>
  $ ul (List.concat [
    Ck_irmin_model.actions node |>  List.map render_action;
    Ck_irmin_model.projects node |> List.map render_project;
  ]) $
>>

let rec render_area node = <:html<
  <li class='area'>$str:Ck_irmin_model.name node$ $add node$</li>
  $render_subareas node$
>>
and render_subareas node = ul (List.concat [
  Ck_irmin_model.actions node  |> List.map render_action;
  Ck_irmin_model.projects node |> List.map render_project;
  Ck_irmin_model.areas node    |> List.map render_area;
])

let all_areas_and_projects m =
  Ck_irmin_model.all_areas_and_projects m
  |> List.map (fun node ->
    let path = Ck_irmin_model.full_name node in
    let uuid = Ck_irmin_model.uuid node in
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

let render_main m =
  let root = Ck_irmin_model.root m in <:html<
  <html class="no-js" lang="en">
    <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>Cue-keeper</title>
      <link rel="stylesheet" href="css/normalize.css" />
      <link rel="stylesheet" href="css/foundation.css" />
      <link rel="stylesheet" href="css/style.css" />
      <script src="js/vendor/modernizr.js"/>
    </head>
    <body>

      <dl class="sub-nav">
        <dd class="active"><a href="#">Process</a></dd>
        <dd><a href="#">Work</a></dd>
        <dd><a href="#">Review</a></dd>
        <dd><a href="#">Contact</a></dd>
      </dl>

      <div class='row'>
        <div class="medium-6 columns">
          $ render_subareas root $
        </div>
        <div class="medium-6 columns">
          $ render_new_action m $
        </div>
      </div>

      <script src="js/vendor/jquery.js"/>
      <script src="js/foundation.min.js"/>
      <script src="js/foundation/foundation.topbar.js"/>
      <script>
        $(document).foundation();
      </script>
    </body>
  </html>
>>
