let applique2 = fun x -> fun f -> (f ( f x));;
let applique2_2 = fun a -> fun b -> (a (a b));;
let rec applique_n = fun n -> fun (f:int->int) -> 
if n = 0 then (fun x -> x) 
else fun x -> f ((applique_n (n - 1) f) x);;
