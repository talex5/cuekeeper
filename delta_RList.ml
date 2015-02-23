(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (Item : Set.OrderedType) (Input : Set.S with type elt = Item.t) = struct
  (* Generate a patch from xs to ys *)
  let rec diff acc i xs ys =
    let open ReactiveData.RList in
    match xs, ys with
    | [], [] -> List.rev acc
    | _::xs, [] -> diff (R i :: acc) i xs []
    | [], y::ys -> diff (I (i, y) :: acc) (i + 1) [] ys
    | x::xs, y::ys ->
        let d = Item.compare x y in
        if d < 0 then
          diff (R i :: acc) i xs (y::ys)
        else if d > 0 then
          diff (I (i, y) :: acc) (i + 1) (x::xs) ys
        else
          diff acc (i + 1) xs ys

  let make ?init s =
    let init =
      match init with
      | None -> React.S.value s
      | Some i -> i in
    let current = ref (Input.elements init) in
    let e = React.S.changes s
      |> React.E.map (fun new_set ->
          (* Calculate the changes need to update the current value to [new_s]. *)
          let new_list = Input.elements new_set in
          let patch = ReactiveData.RList.Patch (diff [] 0 !current new_list) in
          current := new_list;
          patch
      ) in
    ReactiveData.RList.make_from !current e
end
