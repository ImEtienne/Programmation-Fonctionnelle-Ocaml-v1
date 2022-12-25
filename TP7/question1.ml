let rec  positions_paires = fun x ->
 match x with
 | [] -> []
 | _ :: [] -> x
 | t1::t2::q -> t1 :: positions_paires q;;
 
 #trace positions_paires;;

 positions_paires [ 1 ; 12 ; 3 ; 4 ; 7 ];;


 let rec positions_impaires = fun e ->
  match e with
  | t1::t2::q -> t2 :: positions_impaires q
  | _ -> [];;

  #trace positions_impaires;;

  positions_impaires [ 1 ; 12 ; 3 ; 4 ; 7 ];;

let rec coupe = fun x ->
match x with
|[] -> [],[]
| t1 :: [] -> x, []
|t1::t2::q -> let (l1,l2)= coupe q in 
  t1::l1,t2::l2;;

let rec fusion = fun x -> fun y ->
match x, y with
|[],_ -> y
|_,[] -> x
|t1::q1, t2::q2 -> if t1 <t2 then
                    t1::(fusion q1 y)
                    else
                     t2::(fusion x q2);;

let rec tri_fusion = fun z ->
match z with
| [] -> []
|t::[] -> z
|_ -> 
      begin
      let (l1,l2) = coupe z in
      fusion (tri_fusion(l1))(tri_fusion(l2));
end;;


