type directive = TurnLeft | TurnRight
| StepForward of int
| StepBackward of int ;;

type path = directive list ;;

let sample_path = ( StepForward 1 :: StepForward 2:: TurnLeft ::
  StepBackward 3 :: TurnLeft :: StepForward 1:: []);;

let inverse direct = 
  match direct with
    | StepForward x -> StepBackward x
    | StepBackward x -> StepForward x
    | TurnLeft -> TurnRight
    | TurnRight -> TurnLeft;;

let rec string_of_path path = 
  match path with
    | [] -> ""
    | StepForward x::[] -> "Avancer de " ^ string_of_int x ^ " pas"
    | StepBackward x::[] -> "Reculer de " ^ string_of_int x ^ " pas"
    | TurnLeft::[] -> "Tourner à gauche"
    | TurnRight::[] -> "Tourner à droite"
    | StepForward x::q -> "Avancer de " ^ string_of_int x ^ " pas ; " ^ string_of_path q
    | StepBackward x::q -> "Reculer de " ^ string_of_int x ^ " pas ; " ^ string_of_path q
    | TurnLeft::q -> "Tourner à gauche ; " ^ string_of_path q
    | TurnRight::q -> "Tourner à droite ; " ^ string_of_path q;;

string_of_path sample_path;;


type orientation = East | West | North | South;;
type hunter = int * int * orientation;;

let string_of_hunter chasseur = let (x,y,dir) = chasseur in
  match dir with
    | South -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ",sud)"
    | North -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ",nord)"
    | West -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ",ouest)" 
    | East -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ",est)";;

let tourner_a_droite dir =
  match dir with
    | East -> South
    | South -> West
    | West -> North
    | North -> East;;

let tourner_a_gauche dir =
  match dir with
    | East -> North
    | North -> West
    | West -> South
    | South -> East;;

let move chasseur direction = let (x,y,dir) = chasseur in
  match direction with
    | StepForward a -> (match dir with
      | North -> (x,(y+a),dir)
      | East -> ((x+a),y,dir)
      | South -> (x,(y-a),dir)
      | West -> ((x-a),y,dir))
    | StepBackward a -> (match dir with
      | North -> (x,(y-a),dir)
      | East -> ((x-a),y,dir)
      | South -> (x,(y+a),dir)
      | West -> ((x+a),y,dir))
    | TurnLeft -> (x,y, tourner_a_gauche dir)
    | TurnRight -> (x,y, tourner_a_droite dir);;

let rec finally chasseur chemin = let (x,y,dir) = chasseur in
  match chemin with
    | [] -> (x,y,dir)
    | x::q -> finally (move chasseur x) q;;


type obstacles = (int * int) list;;

let mouvement direction = 
  match direction with
    | StepForward _ -> true
    | StepBackward _ -> true
    | TurnLeft -> false
    | TurnRight -> false;;

let move_with_obstacles obs chasseur direction =
  let (xm,ym,dirm) = move chasseur direction in
    match obs with
      | [] -> (xm,ym,dirm)
      | (obsx, obsy)::q -> (match direction with
        | StepForward a -> (match dirm with
          | North -> (x,(y+a),dirm)
          | East -> ((x+a),y,dirm)
          | South -> (x,(y-a),dirm)
          | West -> ((x-a),y,dirm))
        | StepBackward a -> (match dir with
          | North -> (x,(y-a),dirm)
          | East -> ((x-a),y,dirm)
          | South -> (x,(y+a),dirm)
          | West -> ((x+a),y,dirm))
        | TurnLeft -> (xm,ym, tourner_a_gauche dir)
        | TurnRight -> (xm,ym, tourner_a_droite dir)
      );;
        (*if ((xm < obsx || ym < obsy) || (xm > obsx || ym > obsy)) && mouvement direction then chasseur
        else (xm,ym,dirm);;*)
            

          


      
  