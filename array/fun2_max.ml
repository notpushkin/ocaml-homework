let rec fun2_max f bx ex by ey =
  let depth = 0.001 in

  let max2 foo bar =
    if foo > bar then foo else bar
  in

  let rec fun_max f b e =
    (*Printf.printf "X = %f; Y: %f..%f\n" bx b e;*)
    if (e-.b) < depth then
      f b
    else
      max2
        (fun_max f b (e-.((e-.b)/.2.)))
        (fun_max f (b+.((e-.b)/.2.)) e)
  in

  if (ex-.bx) < depth then
    fun_max (f bx) by ey
  else
    max2
      (fun2_max
        f
        bx (ex-.((ex-.bx)/.2.))
        by ey)
      (fun2_max
        f
        (bx+.((ex-.bx)/.2.)) ex
        by ey)
;;

(* DEBUG *)

print_float (
  fun2_max
    (fun x y -> x +. y)
    0. 1.
    0. 2.
);;