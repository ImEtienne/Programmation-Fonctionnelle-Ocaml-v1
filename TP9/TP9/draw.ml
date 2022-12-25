open DrawingType
      
let simple_triangle =
  let pi23 = 2.0 *. pi /. 3.0 in
    Forward 25.0 :: Turn pi23 ::
    Forward 25.0 :: Turn pi23 ::
    Forward 25.0 :: []

let draw = fun n ->
  simple_triangle
