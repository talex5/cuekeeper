(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

let () = Log.(set_log_level INFO)

module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)
module Main = Server.Make(Store)
