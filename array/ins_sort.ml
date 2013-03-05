let print_array f a =
	print_string "[|";
	Array.iter (fun i -> f i; print_string "; ") a;
	print_string "|]";;

let ins_sort_array a =
	let swap (p,q) =
    let t = a.(p) in a.(p) <- a.(q); a.(q) <- t in

	for i = 1 to (Array.length a)-1 do
	  for j = (i-1) downto 0 do
	    if a.(j) > a.(j+1) then
	      swap (j,j+1)
	    done
	  done;;

(* DEBUG *)

let a = [|8; 4; 8; 1|];;
ins_sort_array a;;

print_array (print_int) a;;