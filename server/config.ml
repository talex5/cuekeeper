(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Mirage

let main =
  foreign
    ~deps:[abstract nocrypto]
    "Unikernel.Main" (stackv4 @-> kv_ro @-> pclock @-> job)

let conf = crunch "conf"

let packages = [
  package "irmin" ~sublibs:["mem"];
  package "tls";
  package "cohttp-mirage";
]

let () =
  register ~packages "cuekeeper" [
    main $ generic_stackv4 default_network $ conf $ default_posix_clock
  ]
