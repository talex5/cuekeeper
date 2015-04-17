(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module Gui_tree_data : GUI_DATA

module Make (M : Ck_model_s.MODEL with type gui_data = Gui_tree_data.t) : sig
  val make_top : M.t -> [> `Div ] Tyxml_js.Html5.elt list
end
