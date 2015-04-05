(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv
open Unix

type time_unit =
  | Day
  | Week
  | Month
  | Year
  with sexp

type user_date = (int * int * int) with sexp_of

let of_tm tm =
  (tm.tm_year + 1900, tm.tm_mon, tm.tm_mday)

let mkt (y, m, d) =
  {
    tm_year = y - 1900;
    tm_mon = m;
    tm_mday = d;
    tm_hour = 0;
    tm_min = 0;
    tm_sec = 0;
    (* (these are ignored) *)
    tm_isdst = false;
    tm_wday = 0;
    tm_yday = 0;
  } |> mktime

let tm_of user_date = mkt user_date |> snd

let of_unix_time t =
  of_tm (localtime t)

let unix_time_of d =
  mkt d |> fst

let make ~year ~month ~day =
  mkt (year, month, day)
  |> snd
  |> of_tm

let compare (ay, am, ad) (by, bm, bd) =
  match compare ay by with
  | 0 ->
    begin match compare am bm with
    | 0 -> compare ad bd
    | x -> x end
  | x -> x

let string_of_time_unit = function
  | Day -> "day"
  | Week -> "week"
  | Month -> "month"
  | Year -> "year"

let string_of_day = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> "XXX"

let string_of_unix_time date =
  let tm = localtime date in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min (string_of_day tm.tm_wday)

let string_of_user_date date =
  let tm = tm_of date in
  Printf.sprintf "%04d-%02d-%02d (%s)"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday (string_of_day tm.tm_wday)

let user_date_of_sexp =
  let open Sexplib.Type in function
  | Atom _ as x -> <:of_sexp<float>> x |> of_unix_time (* Old format *)
  | List _ as x -> <:of_sexp<int * int * int>> x

type repeat = {
  repeat_n : int;
  repeat_unit : time_unit;
  repeat_from : user_date;
} with sexp

let make_repeat ~from:repeat_from repeat_n repeat_unit =
  assert (repeat_n > 0);
  { repeat_n; repeat_unit; repeat_from }

let next_repeat ~now r =
  let n = r.repeat_n in
  assert (n > 0);
  let rec aux d =
    if compare d now > 0 then d
    else (
      let (year, month, day) = d in
      let next_d =
        match r.repeat_unit with
        | Day -> make ~year ~month ~day:(day + n)
        | Week -> make ~year ~month ~day:(day + 7 * n)
        | Month -> make ~year ~month:(month + n) ~day
        | Year -> make ~year:(year + n) ~month ~day in
      aux next_d
    ) in
  aux r.repeat_from

let string_of_repeat spec =
  let units = string_of_time_unit spec.repeat_unit in
  match spec.repeat_n with
  | 1 -> Printf.sprintf "every %s" units
  | n -> Printf.sprintf "every %d %ss" n units
