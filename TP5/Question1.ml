let singleton = fun x -> (x :: []);;

singleton 1;;

let positif = fun n -> 
  if n <0 then [] 
  else (n ::[]);;

positif (10);;

let rec rebour = fun f ->
  if f = 0 then (f::[]) 
  else  f :: rebour(f-1);;
#trace rebour;;
  rebour 10;;
  

let rec compteur_aux = fun x -> fun y -> 
  if x = y then x ::[]
  else  x :: compteur_aux (x+1) y ;;
#trace compteur_aux;;
compteur_aux 1 5;;

let compteur = fun f -> 
  if f = 1 then f::[]
  else (compteur_aux (f-f+1) f);;

  compteur 10;;

let head = function
  | []       -> (-1)
  | t::q -> t;;
  head 1;;

let tail = function
  |[]         -> []
  |t::q -> q;;

let head_opt = function
| [] -> None
| t::q -> Some t;;

let tail_opt = function
| [] -> None
| t::q -> Some q;;

let cmp_firsts = fun f -> fun n ->
  let rec  z f n x = if x = n then [f n] else f x :: z f n (x+1)in
  if n=0 then [] 
  else z f n 1;;

let cmp_all = fun n ->
  let rec f x n = if x = n then [n] 
  else x :: f(x+1)n in
  let rec z f n x = if x = n then f 1 n :: [] 
  else f 1 x :: z f n(x+1) in z f n 1;;

let rec cmp_firstsB = fun f -> fun n -> fun i ->
  if i=n  then (f n) :: [] 
  else  (f n) :: cmp_firstsB f (n-1);;

  cmp_firstsB (fun n ->n) 5;;

let is_letter (c:char): bool =
  let x = (int_of_char c) in
  if (x >= 65 && x <= 90) || (x >=97 && x <= 122) then true
  else false;;

let is_capital (c:char):bool = 
  let x = (int_of_char c) in
  if (x >= 65 && x <= 90) then true
  else false;;

let cmp_firsts = fun f -> fun n ->
  let rec z f n x = if (x=n) then [f n] f x :: z f n (x+1) in
  if n=0 then [] else z f n 1;;
let cmp_all = fun n ->
  let rec = fun 



