(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (M : Ck_model_s.MODEL) : sig
  val make_top : M.t -> [> `Div | `Dl ] Tyxml_js.Html5.elt list
end
