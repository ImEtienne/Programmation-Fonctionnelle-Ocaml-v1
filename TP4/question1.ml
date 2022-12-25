let rec fibonacci_1 = fun f -> 
  if f <=1 then 1 
  else fibonacci_1(f-1) + fibonacci_1(f-2);;

  fibonacci_1 10;;

  let rec fibonacci_2 = fun f ->
    if f <=1 then (f, 1)
      else 
        let (x , y) = fibonacci_2 (f-1) in
            (y, x+y);;