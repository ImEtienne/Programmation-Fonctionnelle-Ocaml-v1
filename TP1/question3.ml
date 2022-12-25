let min_arg2 = fun f ->
min_arg2 (f 1) (f 2);;

let min_arg3= fun f ->
  let app2 = min_arg2 f in
  if f app2 < f 3 then app2
  else 3;;

let min_arg4 = fun f ->
  let app3 = min_arg3 f in
  if f app3 < f 4 then app3
  else 4;;

let rec min_arg_n = fun n -> fun f ->
  if n=1 then 1
  else  
  let appm1 = min_arg_n (n-1) f in
  if f appm1 < f n then appm1
  else n;;

let applique2 = fun f -> fun x -> f ( f x);;

