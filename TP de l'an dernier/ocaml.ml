let pow7 = fun n -> n * (n * n) * ((n * n) * (n * n));;
let xor = fun b0 -> fun b1 -> (b0 && not b1) || (b1 && not b0);;
let sgn = fun x -> if x > 0. then 1. else if x < 0. then -1. else 0.;;
let abs = fun n -> n * (int_of_float (sgn (float_of_int n)));;
let perimeter = fun h -> fun w -> (h +. w) *. 2.;;
let arg_min2 = fun (arg1:int) -> fun (arg2:int) -> if arg1 < arg2 then 1 else 2;;
let arg_min3 = fun arg1 -> fun arg2 -> fun (arg3:int) -> if arg3 < arg1 && arg3 < arg2 then 3 else arg_min2 arg1 arg2;;
let arg_min4 = fun arg1 -> fun arg2 -> fun arg3 -> fun (arg4:int) -> if arg4 < arg1 && arg4 < arg2 && arg4 < arg3 then 4 else arg_min3 arg1 arg2 arg3;;
let scalaire = fun x1 -> fun y1 -> fun x2 -> fun y2 -> x1 *. x2 +. y1 *. y2;;
let proj_diag = fun x -> fun y -> let s = ((sqrt 2.) /. 2.) in scalaire s s x y;;

let opp_abs = fun n -> - (abs n);;
let opp_pred = fun n -> - (pred n);;
let opp_succ = fun n -> - (succ n);;
let opp_fct = fun f -> fun (n:int) -> - (f n);;

let sum_fct = fun f -> fun g -> fun (n:int) -> (f n) + (g n);;
let cmp_fct = fun f -> fun g -> fun a -> (f a) = (g a);;
let combine2 = fun (c:'a->'a->'b) -> fun f -> fun g -> fun a -> c (f a) (g a);;

let comp = fun (f:'b->'c) -> fun g -> fun a -> f (g a);;

let min_arg2 = fun (f:int->int) -> if (f 2) < (f 1) then 2 else 1;;
let min_arg3 = fun f -> if (f 3) < (f 1) && (f 3) < (f 2) then 3 else min_arg2 f;;
let min_arg4 = fun f -> if (f 4) < (f 1) && (f 4) < (f 2) && (f 4) < (f 3) then 4 else min_arg3 f;;
let rec min_arg_n = fun n -> fun (f:int->int) -> if n = 1 then 1 else let k = min_arg_n (n - 1) f in if (f k) > (f n) then n else k;;

let applique2 = fun x -> fun (f:int->int) -> f (f x);;
let applique2_2 = fun (f:int->int) -> fun x -> f (f x);;
let rec applique_n = fun n -> fun (f:int->int) -> if n = 0 then (fun x -> x) else fun x -> f ((applique_n (n - 1) f) x);;