let print_array f a =
  print_string "[|";
  Array.iter (fun i -> f i; print_string "; ") a;
  print_string "|]";;

let heap_sort_array a =
  let len = Array.length a in

  let swap p q =
    let t = a.(p) in a.(p) <- a.(q); a.(q) <- t in

  let sift k l =
    (* k --  *)
    let rec check x y =
      if 2*x+1 < l then
        let ch =
          if y < l-1 && a.(y) < a.(y+1) then y+1 else y in
        if a.(x) < a.(ch) then (swap x ch; check ch (2*ch+1)) in
    check k (2*k+1) in

  (* Building a heap *)
  for start = (len/2)-1 downto 0 do
    sift start len;
  done;
  
  (* Sorting it *)
  for term = len-1 downto 1 do
    swap term 0; (* FOA, put term to the very beginning of array *)
    sift 0 term; (* Then repair the heap *)
  done;;

(* DEBUG *)

let a = [|8; 4; 8; 1|];;
heap_sort_array a;;

print_array (print_int) a;;