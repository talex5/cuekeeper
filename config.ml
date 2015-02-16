(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Mirage

let main =
  foreign
    ~libraries:["cow.syntax";"cowabloga";"sexplib.syntax";"uuidm";"irmin.unix"]
    ~packages:["cow";"cowabloga";"sexplib";"uuidm";"irmin"]
    "Unikernel.Main"
    (console @-> http @-> kv_ro @-> job)

let net =
  try match Sys.getenv "NET" with
    | "socket" -> `Socket
    | _        -> `Direct
  with Not_found -> `Socker

let stack console = socket_stackv4 console [Ipaddr.V4.any]

let server = conduit_direct (stack default_console)
let http = 
  http_server (`TCP (`Port 8080)) server

let () =
  let tracing = None in
  (* let tracing = mprof_trace ~size:10000 () in *)
  register ?tracing "cuekeeper" [
    main $ default_console $ http $ crunch "resources"
  ]
