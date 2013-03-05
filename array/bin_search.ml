(* bin_find.ml v2 * Ale110 * WTFPL2 *)

let bin_search a x = 
  let rec search_between m n =
    if m >= n then
      None
    else
      let mn = (m+n)/2 in
      let comp = compare x a.(mn) in
      if comp = 0 then
        Some mn
      else if comp > 0 then
        search_between (mn+1) n
      else
        search_between m mn
  in
  search_between 0 (Array.length a);;


let bin_find = bin_search;;

(* DEBUG *)

let print_option f o =
  match o with
    | None -> print_string "None"
    | Some z -> print_string "Some "; f z;;

print_option
  (print_int)
  (bin_search [|1; 4; 88; 228; 1488; 265265|] 88);;