let is_empty = fun (a,b) -> if a > b then true else false;;
let inclus = fun x -> fun (a,b) -> if x > a && x < b then true else false;;
let intersection = fun (a,b) -> fun (c,d)->  if a < c && b < d then (a,d) 
  else if a < c && d<b then (c,d) 
  else if c<a && b<d then (a,b) 
  else (a,d);;

    