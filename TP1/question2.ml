let opp_abs = fun n -> -n;;
opp_abs (-5);;
let opp_pred = fun n -> - (pred n);;
let opp_succ = fun n -> -(succ n);;
let opp_fct = fun f -> fun n ->
  - (f n);;
let opp_abs_2 = opp_fct abs;;