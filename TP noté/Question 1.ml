let num x1 y1 x2 y2 = abs_float (x2 -. x1.) (y2 -. y1);;


let rec binom = fun n -> fun k ->
  if (k = 0 || n = k) then (0,1)
  else binom(n+1, k+1)= binom (n, k) + binom (n, k+1);;

let rec double_list = fun a ->



let rec min_aux = fun a ->

   