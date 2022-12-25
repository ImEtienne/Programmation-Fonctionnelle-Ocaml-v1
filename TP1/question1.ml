let pw7 = fun n -> 
 let n2 = n*n in 
 let n4 = n2 * n2 in 
  n * n2 * n4;;

let xor = fun a -> fun b ->
  (a || b) && (not (a && b));;

let perimeter = fun length -> fun width ->
  2. *. (length +. width);;

let arg_min2 = fun i -> fun j ->
  if i < j then 1
  else 2;;

let arg_min3 = fun i -> fun j -> fun k ->
  if i < j && i < k then 1
  else 1+ (arg_min2 j k);;

let arg_min4 = fun i1 -> fun i2 -> fun i3 -> fun i4 ->
  if i1 < i2 && i1 && i3 && i1 < i4 then 1
  else 1 + (arg_min3 i2 i3 i4);;

let scalaire = fun x1 -> fun y1 -> fun x2 -> fun y2 ->
  x1 *. x2 +. y1 *. y2;;

let proj_diag = fun x -> fun y ->
  let sq2 = sqrt 2.0 /. 2.0 in
  scalaire x y sq2 sq2;;