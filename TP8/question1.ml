type directive = 
  | TurnLeft 
  | TurnRight
  | StepForward of int
  | StepBackward of int ;;

type path = directive list;;
let sample_path =  ( StepForward 1 :: StepForward 2:: TurnLeft ::
StepBackward 3 :: TurnLeft :: StepForward 1:: []);;

let inverse = fun x ->
match x with
  | TurnRight -> TurnLeft
  | TurnLeft -> TurnRight
  | StepForward x -> StepBackward x
  | StepBackward x -> StepForward x ;;

let rec string_of_path = fun x ->
match x with 
| StepForward x -> "avancer de " ^ (string_of_path x) ^ " pas ;"
| StepBackward x -> "reculer de " ^ (string_of_int x) ^ " pas ; "
| [TurnLeft] -> "tournez a gauche"
| [TurnRight] -> "Tournez a droite"
| StepForward n::q -> ("avancer de " ^ (string_of_int n) ^ " pas ;") ^ string_of_path q ^ " pas" 
|TurnRight :: q -> ("Tourner à droite ; ") ^ string_of_path q
|TurnLeft :: q -> ("Tourner à gauche ; ") ^ string_of_path q
| StepBackward  n::q -> ("reculer de " ^ (string_of_int n) ^ " pas; ") ^ string_of_path q ^ " pas" ;;

    