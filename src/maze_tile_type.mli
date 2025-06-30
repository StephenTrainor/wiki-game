type t =
  | WALL
  | OPEN_SPACE
  | START
  | END
  | IN_SOLUTION_PATH

val open_tiles : t -> t -> bool
