open Lwt
open Cohttp
open Cohttp_lwt_unix
(* open Yojson *)
open Soup
open Printf
(* open Dotenv *)

(* Define a record type for match data *)
type match_data = {
  score: string;
  teams: string list;
  link: string;
}

(* Function to make HTTP GET request *)
let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

(* Function to parse HTML and extract football match data, sorted by score *)
let parse_html html_string =
  let document = Soup.parse html_string in
  let results = ref [] in
  
  (* Find all match elements (li.partido) *)
  document $$ "li.partido" |> to_list |> List.iter (fun match_el ->
    (* Extract the result element *)
    match match_el $? ".resultado" with
    | None -> () (* Skip if no result element *)
    | Some res_el -> 
        (* Extract the link *)
        let link = match res_el $? "a" with
          | None -> ""
          | Some a -> match attribute "href" a with
              | None -> ""
              | Some href -> href
        in
        
        (* Extract the score *)
        let score = match res_el $? ".num" with
          | None -> "0"
          | Some num -> texts num |> String.concat ""
        in
        
        (* Extract team names from img titles *)
        let teams = match_el $$ ".equipo > img" |> to_list |> 
          List.filter_map (fun img -> attribute "title" img) in
        
        (* Add to results *)
        results := { score; teams; link } :: !results
  );
  
  (* Sort results by score (descending) *)
  let sorted_results = List.sort (fun a b -> 
    (* Compare scores in descending order *)
    let score_a = try float_of_string a.score with _ -> 0.0 in
    let score_b = try float_of_string b.score with _ -> 0.0 in
    compare score_b score_a  (* Note: b compares to a for descending order *)
  ) !results in
  
  sorted_results

(* Function to format match data into readable text *)
let format_match_data results =
  String.concat "\n\n" (
    List.mapi (fun idx match_data ->
      let teams_str = String.concat " vs " match_data.teams in
      sprintf "Match #%d\n%s\nScore: %s\nLink: %s"
        (idx + 1)
        teams_str
        match_data.score
        match_data.link
    ) results
  )

(* Function to send email via SendGrid *)
let send_email results =
  (* Read environment variables *)
  let api_key = try Sys.getenv "SENDGRID_API_KEY" with Not_found -> failwith "SENDGRID_API_KEY not set" in
  
  let sender = try Sys.getenv "SENDER_EMAIL" with Not_found -> failwith "SENDER_EMAIL not set" in
  let recipient_str = try Sys.getenv "RECIPIENT_EMAIL" with Not_found -> failwith "RECIPIENT_EMAIL not set" in
  
(* Parse recipient string (comma or semicolon separated) *)
let recipients = recipient_str 
|> Str.split (Str.regexp "[,;]") 
|> List.map String.trim
|> List.filter (fun s -> String.length s > 0)
in

  (* Format current date/time for email subject *)
  let now = Unix.time () |> Unix.gmtime in
  let date_str = sprintf "%04d-%02d-%02d %02d:%02d UTC" 
    (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday now.tm_hour now.tm_min in
  
  (* Build the email content - format match data into readable text *)
  let content = format_match_data results in

  (* Create recipient list for SendGrid *)
  let recipient_json = `List (
    List.map (fun email -> `Assoc [("email", `String email)]) recipients
  ) in
  
  (* Create SendGrid API request payload *)
  let payload = `Assoc [
    
    ("to", recipient_json);
    ("from", `Assoc [
      ("email", `String sender)
    ]);
    ("subject", `String (sprintf "Best NBA Games today - %s" date_str));
    ("text", `String content)
  ] in
  
  (* Convert payload to JSON string *)
  let payload_str = Yojson.Safe.to_string payload in
  
  (* Print request information instead of sending *)
  Printf.printf "\n======= MailerSend API Request (Preview) =======\n";
  Printf.printf "URL: https://api.mailersend.com/v1/email/\n";
  Printf.printf "Method: POST\n";
  Printf.printf "Headers:\n";
  Printf.printf "  Authorization: Bearer %s...\n" (String.sub api_key 0 (min 5 (String.length api_key)) ^ "...");
  Printf.printf "  Content-Type: application/json\n";
  Printf.printf "  X-Requested-With: XMLHttpRequest\n";
  

  Printf.printf "Body:\n%s\n" (Yojson.Safe.pretty_to_string payload);
  Printf.printf "=================================================\n";
  
  

  
  (* Create headers *)
  let headers = Header.init ()
    |> fun h -> Header.add h "Authorization" ("Bearer " ^ api_key)
    |> fun h -> Header.add h "X-Requested-With" "XMLHttpRequest"
    |> fun h -> Header.add h "Content-Type" "application/json" in
  
  (* Send request to SendGrid API *)
  let uri = Uri.of_string "https://api.mailersend.com/v1/email" in
  let body = Cohttp_lwt.Body.of_string payload_str in
  
  Client.post ~headers ~body uri >>= fun (resp, body) ->
    let status = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >>= fun body_str ->  (* Changed to >>= and renamed variable *)
      if status >= 200 && status < 300 then (
        Printf.printf "Email sent successfully (Status: %d)\n" status;
        Lwt.return_unit  (* Add return unit at the end *)
      ) else (
        Printf.printf "Failed to send email (Status: %d): %s\n" status body_str;  (* Use body_str *)
        Lwt.return_unit  (* Add return unit at the end *)
        )

(* Main function *)
let main () =
  Dotenv.export () |> ignore;
  let url = "https://lacronicadesdeelsofa.com/sofialert/" in
  Lwt_main.run (
    print_endline "Fetching HTML...";
    get_json url >>= fun html_string ->
    print_endline "Parsing HTML...";
    let results = parse_html html_string in
   
    (* Write results to a file *)
    let formatted_data = format_match_data results in
    Lwt_io.with_file ~mode:Lwt_io.Output "scraping_results.txt" (fun oc ->
      Lwt_io.write oc formatted_data
    ) >>= fun () ->
    
    Lwt_io.printl "Results written to scraping_results.txt" >>= fun () ->
    
    (* Send email with results *)
    Lwt_io.printl "Sending email..." >>= fun () ->
    send_email results
  )

(* Entry point *)
let () = main ()