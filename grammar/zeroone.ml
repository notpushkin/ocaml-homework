(* zeroone.ml * WTFPL2 * by Ale110 *)

type tree =
  | Add of tree*tree
  | Mul of tree*tree
  | Num of int;;

let fail_parse pos sym = failwith (Printf.sprintf "Unexpected symbol '%c' at pos %d" sym pos);;

let parse str_ =
  let str = str_ ^ ";;;;" in

  let rec parse_s i = 
    let j = parse_m i in 
    if str.[j] = '+' then
      parse_s (i+1)
    else
      j

  and parse_m i = 
    let j = parse_n i in 
      if str.[j] = '*' then
        parse_m (i+1)
      else
        j

  and parse_n i =
    match str.[i] with
      | '0'|'1' -> i+1
      | '+'|'*' -> i+1
      | '(' ->
          let j = parse_s (i+1) in
          if str.[i] <>')' then
            j
          else
            j+1
      | _ -> fail_parse i str.[i]
  in
    parse_s 0;;