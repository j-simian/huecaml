open! Core
open! Async
open Cohttp
open Cohttp_async

let discover () =
  let uri = Uri.of_string "https://discovery.meethue.com" in
  let%bind resp, body = Client.get uri in
  let status = Response.status resp in
  let%bind body_str = Body.to_string body in
  if Code.is_success (Code.code_of_status status)
  then (
    match Jsonaf.parse body_str with
    | Ok json ->
      (match Jsonaf.list_exn json with
       | entries ->
         let ips =
           List.filter_map entries ~f:(fun entry ->
             match Jsonaf.member "internalipaddress" entry with
             | Some ip -> Some (Jsonaf.string_exn ip)
             | None -> None)
         in
         Deferred.Or_error.return ips
       | exception exn ->
         Deferred.Or_error.error_string
           (sprintf "Failed to parse discovery response: %s" (Exn.to_string exn)))
    | Error err -> Deferred.return (Error err))
  else
    Deferred.Or_error.error_string
      (sprintf "Discovery HTTP %d: %s" (Code.code_of_status status) body_str)
;;

let authenticate ~bridge_ip ~device_type ~app_name =
  let uri = Uri.make ~scheme:"https" ~host:bridge_ip ~path:"/api" () in
  let body_json =
    `Object [ "devicetype", `String (sprintf "%s#%s" app_name device_type) ]
  in
  let body = Cohttp_async.Body.of_string (Jsonaf.to_string body_json) in
  let headers = Header.of_list [ "Content-Type", "application/json" ] in
  let%bind resp, resp_body = Client.post ~headers ~body uri in
  let status = Response.status resp in
  let%bind body_str = Body.to_string resp_body in
  if Code.is_success (Code.code_of_status status)
  then (
    match Jsonaf.parse body_str with
    | Ok json ->
      (match Jsonaf.list_exn json with
       | [ entry ] ->
         (match Jsonaf.member "success" entry with
          | Some success ->
            (match Jsonaf.member "username" success with
             | Some username -> Deferred.Or_error.return (Jsonaf.string_exn username)
             | None ->
               Deferred.Or_error.error_string "No username in success response")
          | None ->
            (match Jsonaf.member "error" entry with
             | Some err ->
               let desc =
                 match Jsonaf.member "description" err with
                 | Some d -> Jsonaf.string_exn d
                 | None -> "unknown error"
               in
               Deferred.Or_error.error_string (sprintf "Bridge error: %s" desc)
             | None ->
               Deferred.Or_error.error_string
                 (sprintf "Unexpected response: %s" body_str)))
       | _ ->
         Deferred.Or_error.error_string
           (sprintf "Unexpected response format: %s" body_str)
       | exception exn ->
         Deferred.Or_error.error_string
           (sprintf "Failed to parse auth response: %s" (Exn.to_string exn)))
    | Error err -> Deferred.return (Error err))
  else
    Deferred.Or_error.error_string
      (sprintf "Auth HTTP %d: %s" (Code.code_of_status status) body_str)
;;
