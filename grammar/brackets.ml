(* brackets.ml * WTFPL2 * by Ale110 *)

type tree = 
  | Round of tree list
  | Square of tree list;;

let parse str_ =
  let str = str_ ^ ";;;;" in
  
  let rec parse_s i =
    match str.[i] with
      | '[' ->
        let (j1, k1) = (parse_s (i + 1)) in
          if str.[j1] != ']' then
            failwith "parse_s [ error"
          else
            let (j2, k2) = parse_s (j1 + 1) in
              (j2, [Square k1] @ k2)

      | '(' ->
        let (j1, k1) = (parse_s (i + 1)) in
          if str.[j1] != ')' then
            failwith "parse_s ( error"
          else
            let (j2, k2) = parse_s (j1 + 1) in
              (j2, [Round k1] @ k2)

      | _ -> (i, [])

  in
    snd (parse_s 0);;

(* DEBUG *)

let print_list f lst = List.iter (fun x -> (f x)) lst;;

let rec print_tree t =
  match t with
    | Round l ->  print_string "("; print_list (print_tree) l; print_string ")"
    | Square l -> print_string "["; print_list (print_tree) l; print_string "]";;

print_list (print_tree) (parse "(([[]](())[[]]))");












