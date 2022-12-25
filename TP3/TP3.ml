
let rec puissance = fun x -> fun i -> if i=0 then 1. else if (i<0) then (1. /. x) *. puissance x (i+1) else x *. (puissance x (i-1));;
let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n-1);;
let rec stars = fun x -> if x = 0 then "" else "*"^stars (x-1);;
let rec compte_a_rebour = fun x -> if x = 0 then (string_of_int x) else (string_of_int x)^" "^ compte_a_rebour(x-1);;
let rec compteur = fun x -> if x = 1  then (string_of_int x) else  compteur (x-1)^" "^(string_of_int x);;
let rec sum_firsts = fun n -> if n = 1 then 1 else n + sum_firsts(n-1);;
let rec fsum_firsts = fun f -> fun n -> if n=1 then f 1 else fsum_firsts f (n-1) + f n;;



let rec fcat_firsts = fun f -> fun n -> if n=1 then f 1 else fcat_firsts f (n -1) ^ (f n);; 
let rec fprint_firsts = fun f -> fun n -> 
if n != 0 then fprint_firsts f (n-1)^ " "^( string_of_int (f n)) else "";;

let rec retourne_aux = fun n -> fun s -> 
    if n < 0 then "" 
    else Char.escaped (String.get s n)^retourne_aux (n-1) s;; 
let retourne (s:string):string = 
    if (String.length s)= 0 then "" 
    else retourne_aux ((String.length s)-1) s;;

    let rec retourne_aux = fun n -> fun s -> 
    if n < 0 then "" 
    else Char.escaped (String.get s n)^retourne_aux (n-1) s;; 
let retourne (s:string):string = 
    if (String.length s)= 0 then "" 
    else retourne_aux ((String.length s)-1) s;;
let rec est_premier_aux = fun n -> fun m->
  if (m = 0) then true
  else if (m = 1) then true
  else begin
      if ((mod) n m = 0) then false    
      else true && (est_premier_aux n (m-1)) end;;

let est_premier = fun x ->
if x >=2 then est_premier_aux x (x-1) else false;;