open! Core

let select_to_list (selector : string) (node_to_select_from : 'a Soup.node)
  : Soup.element Soup.node list
  =
  let open Soup in
  select selector node_to_select_from |> to_list
;;

(*
   let correct_credits_div (element_to_check : Soup.element Soup.node) : bool =
   let open Soup in
   let data_test_id : string option =
   attribute "data-testid" element_to_check
   in
   match data_test_id with
   | None -> false
   | Some test_id ->
   if String.equal test_id "Filmography" then true else false
   ;;

   let get_credits_div (contents : string) : 'a Soup.node option =
   let open Soup in
   let celwidget_divs_list = parse contents |> select_to_list ".celwidget" in
   List.find ~f:correct_credits_div celwidget_divs_list
   ;;

   let get_credits_li_list (contents : string) : 'a Soup.node list =
   match get_credits_div contents with
   | None -> []
   | Some credits_div -> select_to_list "li" credits_div
   ;;

   let select_second_div (li_element : Soup.element Soup.node)
   : Soup.element Soup.node option
   =
   List.nth (select_to_list "div" li_element) 1
   ;;

   let select_link_text (element_to_check : Soup.element Soup.node) : string =
   let open Soup in
   let target_link = select_one "a" element_to_check in
   match target_link with
   | None -> ""
   | Some link -> texts link |> String.concat ~sep:""
   ;;

   let extract_title (li_element : Soup.element Soup.node) : string =
   match select_second_div li_element with
   | None -> ""
   | Some title_container_div -> select_link_text title_container_div
   ;;

   let extract_titles_from_li_list (li_list : Soup.element Soup.node list)
   : string list
   =
   List.map ~f:extract_title li_list (* correct up to this line *)
   |> List.dedup_and_sort ~compare:String.compare
   ;; *)

let extract_title (element_to_extract_from : Soup.element Soup.node) : string
  =
  let open Soup in
  texts element_to_extract_from |> String.concat ~sep:"" |> String.strip
;;

let remove_duplicates l = List.dedup_and_sort ~compare:String.compare l

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  select_to_list "a.ipc-metadata-list-summary-item__t" (parse contents)
  |> List.map ~f:extract_title
  |> remove_duplicates
;;

(* get_credits_li_list contents |> extract_titles_from_li_list *)

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
