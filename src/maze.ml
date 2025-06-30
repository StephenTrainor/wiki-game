open! Core
module G = Graph.Imperative.Graph.Concrete (Int)

type t =
  { grid : Maze_tile_type.t array array
  ; width : int
  ; height : int
  }

let x_y_to_ordinal t (x : int) (y : int) : int = x + (y * t.width)

let ordinal_to_x_y t (ordinal : int) : int * int =
  let divided_out = ordinal / t.width in
  let remainder = ordinal % t.width in
  remainder, divided_out
;;

let set_from_ordinal t (ordinal : int) (tile_type : Maze_tile_type.t) : unit =
  let x, y = ordinal_to_x_y t ordinal in
  t.grid.(x).(y) <- tile_type
;;

let set_from_x_y t (x : int) (y : int) (tile_type : Maze_tile_type.t) : unit =
  t.grid.(x).(y) <- tile_type
;;

let get_from_x_y t (x : int) (y : int) : Maze_tile_type.t = t.grid.(x).(y)

let get_from_ordinal t (ordinal : int) : Maze_tile_type.t =
  let x, y = ordinal_to_x_y t ordinal in
  get_from_x_y t x y
;;

let find_start_in_maze t =
  let open Maze_tile_type in
  let start = ref 0 in
  for x = 0 to t.width - 1 do
    for y = 0 to t.height - 1 do
      match t.grid.(x).(y) with
      | START -> start.contents <- x_y_to_ordinal t x y
      | WALL | OPEN_SPACE | END | IN_SOLUTION_PATH -> ()
    done
  done;
  start.contents
;;

let print_maze t : unit =
  let open Maze_tile_type in
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      printf
        (match get_from_x_y t x y with
         | WALL -> "# "
         | OPEN_SPACE -> ". "
         | START | END | IN_SOLUTION_PATH -> "x ")
    done;
    print_endline ""
  done
;;

let populate_array_from_file (input_file : File_path.t) (maze : t) : unit =
  let y = ref 0
  and maze_file_lines =
    In_channel.read_lines (File_path.to_string input_file)
  in
  let rec populate_a_row_of_array (file_lines : string list) : unit =
    match file_lines with
    | [] -> ()
    | line :: rest_of_file_lines ->
      for x = 0 to String.length line - 1 do
        let tile_type : Maze_tile_type.t =
          match String.get line x with
          | 'S' -> Maze_tile_type.START
          | 'E' -> Maze_tile_type.END
          | '.' -> Maze_tile_type.OPEN_SPACE
          | '#' | _ -> Maze_tile_type.WALL
        in
        set_from_x_y maze x !y tile_type
      done;
      incr y;
      populate_a_row_of_array rest_of_file_lines
  in
  populate_a_row_of_array maze_file_lines
;;

let add_bidirectional_edge maze graph p1 p2 =
  let x1, y1 = p1
  and x2, y2 = p2 in
  let ordinal_1 = x_y_to_ordinal maze x1 y1
  and ordinal_2 = x_y_to_ordinal maze x2 y2 in
  G.add_edge graph ordinal_1 ordinal_2;
  G.add_edge graph ordinal_2 ordinal_1
;;

let populate_graph_from_array (graph : G.t) (maze : t) : unit =
  let open Maze_tile_type in
  for x = 0 to maze.width - 2 do
    for y = 0 to maze.height - 2 do
      let current_tile = get_from_x_y maze x y
      and right_tile = get_from_x_y maze (x + 1) y
      and below_tile = get_from_x_y maze x (y + 1) in
      if Maze_tile_type.open_tiles current_tile right_tile
      then add_bidirectional_edge maze graph (x, y) (x + 1, y);
      if Maze_tile_type.open_tiles current_tile below_tile
      then add_bidirectional_edge maze graph (x, y) (x, y + 1)
    done
  done;
  for x = 0 to maze.width - 2 do
    let current_tile = get_from_x_y maze x (maze.height - 1)
    and right_tile = get_from_x_y maze (x + 1) (maze.height - 1) in
    match current_tile with
    | OPEN_SPACE | END | START ->
      (match right_tile with
       | OPEN_SPACE | END | START ->
         G.add_edge
           graph
           (x_y_to_ordinal maze x (maze.height - 1))
           (x_y_to_ordinal maze (x + 1) (maze.height - 1));
         G.add_edge
           graph
           (x_y_to_ordinal maze (x + 1) (maze.height - 1))
           (x_y_to_ordinal maze x (maze.height - 1))
       | WALL | IN_SOLUTION_PATH -> ())
    | WALL | IN_SOLUTION_PATH -> ()
  done;
  for y = 0 to maze.height - 2 do
    let current_tile = get_from_x_y maze (maze.width - 1) y
    and below_tile = get_from_x_y maze (maze.width - 1) (y + 1) in
    match current_tile with
    | OPEN_SPACE | END | START ->
      (match below_tile with
       | OPEN_SPACE | END | START ->
         G.add_edge
           graph
           (x_y_to_ordinal maze (maze.width - 1) y)
           (x_y_to_ordinal maze (maze.width - 1) (y + 1));
         G.add_edge
           graph
           (x_y_to_ordinal maze (maze.width - 1) (y + 1))
           (x_y_to_ordinal maze (maze.width - 1) y)
       | WALL | IN_SOLUTION_PATH -> ())
    | WALL | IN_SOLUTION_PATH -> ()
  done
;;

let create_graph_from_maze_file (input_file : File_path.t) : G.t * t =
  let maze_lines : string list =
    In_channel.read_lines (File_path.to_string input_file)
  in
  let width = String.length (List.hd_exn maze_lines) in
  let height = List.length maze_lines in
  let grid = Array.make_matrix ~dimx:width ~dimy:height Maze_tile_type.WALL
  and graph = G.create () in
  let maze : t = { grid; width; height } in
  populate_array_from_file input_file maze;
  populate_graph_from_array graph maze;
  graph, maze
;;

let rec plot_path_in_maze
          (maze : t)
          (parent : (int, int) Hashtbl_intf.Hashtbl.t)
          (end_ordinal : int)
  : unit
  =
  set_from_ordinal maze end_ordinal IN_SOLUTION_PATH;
  match Hashtbl.find parent end_ordinal with
  | None -> ()
  | Some ordinal -> plot_path_in_maze maze parent ordinal
;;

let solve_maze (input_file : File_path.t) : unit =
  let open Maze_tile_type in
  let end_ordinal = ref (-1) in
  let graph, maze = create_graph_from_maze_file input_file
  and parent = Hashtbl.create (module Int)
  and visited = Hash_set.create (module Int)
  and to_visit = Queue.create () in
  let start = find_start_in_maze maze in
  Queue.enqueue to_visit start;
  let rec bfs () =
    match Queue.dequeue to_visit with
    | None -> ()
    | Some maze_ordinal ->
      Hash_set.add visited maze_ordinal;
      let neighbors : int list =
        G.succ_e graph maze_ordinal |> List.map ~f:G.E.dst
      in
      List.iter neighbors ~f:(fun neighbor ->
        if not (Hash_set.mem visited neighbor)
        then (
          match get_from_ordinal maze neighbor with
          | END ->
            ignore (Hashtbl.add parent ~key:neighbor ~data:maze_ordinal);
            end_ordinal := neighbor
          | START | OPEN_SPACE ->
            ignore (Hashtbl.add parent ~key:neighbor ~data:maze_ordinal);
            Queue.enqueue to_visit neighbor;
            bfs ()
          | _ -> ()))
  in
  bfs ();
  plot_path_in_maze maze parent !end_ordinal;
  print_maze maze
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () -> solve_maze input_file]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
