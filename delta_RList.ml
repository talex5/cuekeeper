(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

module Make (Item : Set.OrderedType) (Input : Map.S with type key = Item.t) = struct
  (* Generate a patch from xs to ys *)
  let rec diff acc i xs ys =
    let open ReactiveData.RList in
    match xs, ys with
    | [], [] -> List.rev acc
    | _::xs, [] -> diff (R i :: acc) i xs []
    | [], y::ys -> diff (I (i, y) :: acc) (i + 1) [] ys
    | x::xs, y::ys ->
        let d = Item.compare (fst x) (fst y) in
        if d < 0 then
          diff (R i :: acc) i xs (y::ys)
        else if d > 0 then
          diff (I (i, y) :: acc) (i + 1) (x::xs) ys
        else if snd x = snd y then
          diff acc (i + 1) xs ys
        else
          diff (U (i, y) :: acc) (i + 1) xs ys  (* Same key, but value has changed. *)

  let make ?init s =
    let init =
      match init with
      | None -> React.S.value s
      | Some i -> i in
    let current = ref (Input.bindings init) in
    let e = React.S.changes s
      |> React.E.map (fun new_set ->
          (* Calculate the changes need to update the current value to [new_s]. *)
          let new_list = Input.bindings new_set in
          let patch = ReactiveData.RList.Patch (diff [] 0 !current new_list) in
          current := new_list;
          patch
      ) in
    ReactiveData.RList.make_from !current e
end
