(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Store = Irmin.Basic(Irmin_unix.Irmin_git.FS)(Irmin.Contents.String)
include Ck_model.Make(Store)
