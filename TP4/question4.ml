let frac_mul = fun (a,b) -> fun (c,d) -> (a*c),(b*d);;
let frac_add = fun (a,b) -> fun (c,d) -> 
    ((a*d)+(b*c) , d*b ) ;;

    let rec pgcd = fun a b ->
    if a = 0 then b
             else pgcd (b mod a) a;;
let frac_irr = fun (a, b) ->
    if b < 0 then (-a / (pgcd (abs a) (abs b)), -b / (pgcd (abs a) (abs b))) 
    else 
        if a < 0 && b < 0 then (-a / (pgcd (abs a) (abs b)), -b / (pgcd (abs a) (abs b)))
    else (a / (pgcd (abs a) (abs b)), b / (pgcd (abs a) (abs b)));;
let frac_eq = fun (a,b) -> fun (c,d) -> if (a,b) == (c,d) then true else false;;


 