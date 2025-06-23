open! Core

(* Gets the "title" node of an HTML page. *)
let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

let%expect_test "get_title" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  print_endline (get_title contents);
  [%expect {| My Blog |}]
;;

(* Gets all of the list items contained in an HTML page. *)
let get_list_items contents : string list =
  let open Soup in
  parse contents
  $$ "li"
  |> to_list
  |> List.map ~f:(fun li ->
    texts li |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "get_list_items" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (get_list_items contents) ~f:print_endline;
  [%expect
    {|
    All feliforms, such as domestic cats, big cats, hyenas, mongooses, civets
    Almost all caniforms, such as the dogs, wolves, foxes, ferrets, seals and walruses
    All cetaceans, such as dolphins, whales and porpoises
    All bats except fruitbats
    The carnivorous marsupials, such as the Tasmania devil
    All birds of prey, such as hawks, eagles, falcons and owls
    All vultures, both old world and new
    Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    |}]
;;

let extract_string_from_first_li_from_node node : string =
  let open Soup in
  let li_elements = select "li" node in
  let first_li_element = to_list li_elements |> List.hd_exn in
  texts first_li_element |> String.concat ~sep:"" |> String.strip
;;

(* Gets the first item of all unordered lists contained in an HTML page. *)
let get_first_item_of_all_unordered_lists contents : string list =
  let open Soup in
  parse contents
  $$ "ul"
  |> to_list
  |> List.map ~f:extract_string_from_first_li_from_node
;;

let%expect_test "get_first_item_of_all_unordered_lists" =
  let contents =
    {|<!DOCTYPE html>
<html>

<head>
  <meta charset="UTF-8">
  <title>Carnivore - Wikipedia</title>
</head>

<body>
  <p>A <b>carnivore</b> is an <a href="/wiki/Animal">animal</a> which eats
    only meat. <b>Predators</b> commonly hunt and kill their own
    prey. <b>Scavengers</b> are carnivores which eat animals they did not
    kill themselves. Carnivores which eat mainly or only insects are
    called <b>insectivores</b>. Carnivores which eat mainly or only fish
    are called <b>piscivores</b>.</p>

  <h2>List of living carnivores</h2>

  <h3>Mammals</h3>
  <ul>
    <li>
      All <a href="/wiki/Feliformia">feliforms</a>, such as domestic cats, big cats, hyenas, mongooses, civets
    </li>
    <li>
      Almost all <a href="/wiki/Caniformia">caniforms</a>, such as the dogs, wolves, foxes, ferrets, seals and walruses
    </li>
    <li>
      All cetaceans, such as dolphins, whales and porpoises
    </li>
    <li>
      All bats except fruitbats
    </li>
    <li>
      The carnivorous marsupials, such as the Tasmania devil
    </li>
  </ul>

  <h3>Birds</h3>
  <ul>
    <li>
      All birds of prey, such as hawks, eagles, falcons and owls
    </li>
    <li>
      All vultures, both old world and new
    </li>
    <li>
      Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    </li>
  </ul>
  <p><a href="/wiki/Talk:Carnivore">Talk</a></p>
</body>

</html>
|}
  in
  List.iter (get_first_item_of_all_unordered_lists contents) ~f:(fun s ->
    print_endline s);
  [%expect
    {|
All feliforms, such as domestic cats, big cats, hyenas, mongooses, civets
All birds of prey, such as hawks, eagles, falcons and owls |}]
;;

(* Gets the first item of the second unordered list in an HTML page. *)
let get_first_item_of_second_unordered_list contents : string =
  let second_of_list (l : 'a list) : 'a = List.nth_exn l 1 in
  let open Soup in
  parse contents
  $$ "ul"
  |> to_list
  |> second_of_list
  |> extract_string_from_first_li_from_node
;;

let%expect_test "get_first_item_of_second_unordered_list" =
  let contents =
    {|<!DOCTYPE html>
<html>

<head>
  <meta charset="UTF-8">
  <title>Carnivore - Wikipedia</title>
</head>

<body>
  <p>A <b>carnivore</b> is an <a href="/wiki/Animal">animal</a> which eats
    only meat. <b>Predators</b> commonly hunt and kill their own
    prey. <b>Scavengers</b> are carnivores which eat animals they did not
    kill themselves. Carnivores which eat mainly or only insects are
    called <b>insectivores</b>. Carnivores which eat mainly or only fish
    are called <b>piscivores</b>.</p>

  <h2>List of living carnivores</h2>

  <h3>Mammals</h3>
  <ul>
    <li>
      All <a href="/wiki/Feliformia">feliforms</a>, such as domestic cats, big cats, hyenas, mongooses, civets
    </li>
    <li>
      Almost all <a href="/wiki/Caniformia">caniforms</a>, such as the dogs, wolves, foxes, ferrets, seals and walruses
    </li>
    <li>
      All cetaceans, such as dolphins, whales and porpoises
    </li>
    <li>
      All bats except fruitbats
    </li>
    <li>
      The carnivorous marsupials, such as the Tasmania devil
    </li>
  </ul>

  <h3>Birds</h3>
  <ul>
    <li>
      All birds of prey, such as hawks, eagles, falcons and owls
    </li>
    <li>
      All vultures, both old world and new
    </li>
    <li>
      Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    </li>
  </ul>
  <p><a href="/wiki/Talk:Carnivore">Talk</a></p>
</body>

</html>
|}
  in
  print_endline (get_first_item_of_second_unordered_list contents);
  [%expect {| All birds of prey, such as hawks, eagles, falcons and owls |}]
;;

(* Gets all bolded text from an HTML page. *)
let get_bolded_text contents : string list =
  let open Soup in
  parse contents
  $$ "b"
  |> to_list
  |> List.map ~f:(fun b_tag ->
    texts b_tag |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "get_bolded_text" =
  let contents =
    {|<!DOCTYPE html>
<html>

<head>
  <meta charset="UTF-8">
  <title>Carnivore - Wikipedia</title>
</head>

<body>
  <p>A <b>carnivore</b> is an <a href="/wiki/Animal">animal</a> which eats
    only meat. <b>Predators</b> commonly hunt and kill their own
    prey. <b>Scavengers</b> are carnivores which eat animals they did not
    kill themselves. Carnivores which eat mainly or only insects are
    called <b>insectivores</b>. Carnivores which eat mainly or only fish
    are called <b>piscivores</b>.</p>

  <h2>List of living carnivores</h2>

  <h3>Mammals</h3>
  <ul>
    <li>
      All <a href="/wiki/Feliformia">feliforms</a>, such as domestic cats, big cats, hyenas, mongooses, civets
    </li>
    <li>
      Almost all <a href="/wiki/Caniformia">caniforms</a>, such as the dogs, wolves, foxes, ferrets, seals and walruses
    </li>
    <li>
      All cetaceans, such as dolphins, whales and porpoises
    </li>
    <li>
      All bats except fruitbats
    </li>
    <li>
      The carnivorous marsupials, such as the Tasmania devil
    </li>
  </ul>

  <h3>Birds</h3>
  <ul>
    <li>
      All birds of prey, such as hawks, eagles, falcons and owls
    </li>
    <li>
      All vultures, both old world and new
    </li>
    <li>
      Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    </li>
  </ul>
  <p><a href="/wiki/Talk:Carnivore">Talk</a></p>
</body>

</html>
|}
  in
  List.iter ~f:(fun s -> print_endline s) (get_bolded_text contents);
  [%expect
    {| 
    carnivore
    Predators
    Scavengers
    insectivores
    piscivores |}]
;;

(* [make_command ~summary ~f] is a helper function that builds a simple HTML parsing
   command. It takes in a [summary] for the command, as well as a function [f] that
   transforms a string (the HTML contents of a page) into a list of strings (the parsed
   results from that HTML page). *)
let make_command ~summary ~f =
  let open Command.Let_syntax in
  Command.basic
    ~summary
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (f contents) ~f:print_endline]
;;

let print_title_command =
  make_command
    ~summary:"print the title from an HTML page"
    ~f:(fun contents -> [ get_title contents ])
;;

let print_list_items_command =
  make_command
    ~summary:"print all list items from an HTML page"
    ~f:get_list_items
;;

let print_first_item_of_all_unordered_lists_command =
  make_command
    ~summary:"print the first item of each unordered list from an HTML page"
    ~f:get_first_item_of_all_unordered_lists
;;

let print_first_item_of_second_unordered_list_command =
  make_command
    ~summary:"print first item of the second unordered list of an HTML page"
    ~f:(fun contents -> [ get_first_item_of_second_unordered_list contents ])
;;

let print_bolded_text_command =
  make_command
    ~summary:"print all bolded text in an HTML page"
    ~f:get_bolded_text
;;

let command =
  Command.group
    ~summary:"lambda soup demo"
    [ "print-title", print_title_command
    ; "print-list-items", print_list_items_command
    ; ( "print-first-item-of-all-unordered-lists"
      , print_first_item_of_all_unordered_lists_command )
    ; ( "print-first-item-of-second-unordered-list"
      , print_first_item_of_second_unordered_list_command )
    ; "print-bolded-text", print_bolded_text_command
    ]
;;
