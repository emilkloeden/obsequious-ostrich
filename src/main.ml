open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe

(* Function to make HTTP GET request *)
let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

(* Function to parse JSON *)
let parse_json json_string =
  try
    let json = from_string json_string in
    `Ok json
  with e ->
    `Error (Printexc.to_string e)

(* Function to extract and print data from parsed JSON *)
let extract_data json =
  match json with
  | `List items ->
      List.iter (fun item ->
        let id = Yojson.Safe.Util.member "id" item |> Yojson.Safe.Util.to_int in
        let title = Yojson.Safe.Util.member "title" item |> Yojson.Safe.Util.to_string in
        Printf.printf "Post ID: %d, Title: %s\n" id title
      ) items
  | _ -> Printf.printf "Expected a JSON list\n"

(* Main function *)
let main () =
  let url = "https://jsonplaceholder.typicode.com/posts" in
  Lwt_main.run (
    print_endline "Fetching data...";
    get_json url >>= fun json_string ->
    print_endline "Parsing JSON...";
    let parsed = parse_json json_string in
    match parsed with
    | `Ok json -> 
        print_endline "Extracting data...";
        extract_data json;
        Lwt.return_unit
    | `Error msg ->
        Printf.printf "Error parsing JSON: %s\n" msg;
        Lwt.return_unit
  )

(* Entry point *)
let () = main ()
