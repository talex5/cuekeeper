module M = Map.Make(String)

type t = string M.t

let of_file path =
  let ch = open_in path in
  let t = ref M.empty in
  try
    while true do
      match String.trim (input_line ch) with
      | "" -> ()
      | line ->
        let i = String.index line ' ' in
        let hash = String.sub line 0 i in
        let label = String.sub line (i + 1) (String.length line - i - 1) in
        t := M.add hash label !t
    done;
    assert false
  with End_of_file ->
    close_in ch;
    !t
                        
let lookup t hash = M.find_opt hash t
