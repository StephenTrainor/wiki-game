open! Core

module My_graph = struct
  module Edge = struct
    module T = struct
      type t = string * string * string [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let of_string s =
      let all_values = String.split ~on:',' s in
      let interstate = String.concat (List.take all_values 1) ~sep:"" in
      let people_along_interstate = List.drop all_values 1 in
      let rec inner_loop (from_city : string) (l : string list) =
        match l with
        | [] -> []
        | head :: tail ->
          (from_city, interstate, head) :: inner_loop from_city tail
      in
      let rec outer_loop (l : string list) : (string * string * string) list =
        match l with
        | [] -> []
        | head :: tail ->
          List.append (outer_loop tail) (inner_loop head tail)
      in
      outer_loop people_along_interstate
    ;;
  end

  type t = Edge.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let edges =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s -> Edge.of_string s)
    in
    Edge.Set.of_list edges
  ;;
end

module Highway_name = struct
  include String

  let default = ""
end

module G = Graph.Imperative.Graph.ConcreteLabeled (String) (Highway_name)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes e = [ `Dir `None; `Label (snd3 e) ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = Printf.sprintf "\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let graph = My_graph.of_file input_file in
        printf !"%{sexp: My_graph.t}\n" graph]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let my_graph = My_graph.of_file input_file in
        let g = G.create () in
        Set.iter my_graph ~f:(fun triplet -> G.add_edge_e g triplet);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          g;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
