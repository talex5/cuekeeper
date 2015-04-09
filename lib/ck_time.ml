(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv

type time_unit =
  | Day
  | Week
  | Month
  | Year
  with sexp

type repeat = {
  repeat_n : int;
  repeat_unit : time_unit;
  repeat_from : float;
} with sexp

(** [next_repeat ~now r] gives the next time that [r.repeat_from] should be set to,
 * by moving [repeat_from] forward in steps of [repeat_n * repeat_unit] until it's
 * after [now]. *)
let next_repeat ~now r =
  let open Unix in
  let n = r.repeat_n in
  assert (n > 0);
  let rec aux d =
    if d >= now then d
    else (
      let tm = localtime d in
      let tm =
        match r.repeat_unit with
        | Day -> {tm with tm_mday = tm.tm_mday + n}
        | Week -> {tm with tm_mday = tm.tm_mday + 7 * n}
        | Month -> {tm with tm_mon = tm.tm_mon + n}
        | Year -> {tm with tm_year = tm.tm_year + n} in
      aux (fst (mktime tm))
    ) in
  aux r.repeat_from
