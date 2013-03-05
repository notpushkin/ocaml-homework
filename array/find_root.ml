(* find_root.ml * Ale110 * WTFPL2 *)

let find_root f be x = 
  let rec search_between m n =
    if m >= n then
      None
    else
      let mn = (m+.n)/.2. in
      let comp = compare x (f mn) in
      if comp = 0 then
        Some mn
      else if comp > 0 then
        search_between mn n
      else
        search_between m mn
  in
  search_between (fst be) (snd be);;

(* DEBUG *)

let print_option f o =
  match o with
    | None -> print_string "None"
    | Some z -> print_string "Some "; f z;;

print_option
  (print_float)
  (find_root
    (fun x -> (3.*.(x*.x)) +. (2.*.x))
    (1., 100.)
    14.88888888
  );;