type t =
  | WALL
  | OPEN_SPACE
  | START
  | END
  | IN_SOLUTION_PATH

let open_tiles t1 t2 =
  match t1 with
  | OPEN_SPACE | END | START ->
    (match t2 with
     | OPEN_SPACE | END | START -> true
     | WALL | IN_SOLUTION_PATH -> false)
  | WALL | IN_SOLUTION_PATH -> false
;;
