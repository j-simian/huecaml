open! Core
open! Async

let ssl_config =
  Conduit_async.V2.Ssl.Config.create ~verify_modes:[] ~verify:(fun _ -> return true) ()
;;

let discover () =
  let uri = Uri.of_string "https://discovery.meethue.com" in
  let%bind resp, body = Cohttp_async.Client.get ~ssl_config uri in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status))
  then (
    let http_error_code = Cohttp.Code.code_of_status status in
    Deferred.Or_error.error_s
      [%message
        "Error during HTTP connection for discovery" (http_error_code : int) body_str])
  else (
    let%bind.Deferred.Or_error json = Jsonaf.parse body_str |> Deferred.return in
    match Jsonaf.list json with
    | None -> Deferred.Or_error.error_s [%message "Failed to parse discovery response"]
    | Some entries ->
      let ips =
        List.filter_map entries ~f:(fun entry ->
          match Jsonaf.member "internalipaddress" entry with
          | Some ip -> Some (Jsonaf.string_exn ip)
          | None -> None)
      in
      Deferred.Or_error.return ips)
;;

let authenticate ~bridge_ip ~device_type ~app_name =
  let uri = Uri.make ~scheme:"https" ~host:bridge_ip ~path:"/api" () in
  let body_json =
    `Object [ "devicetype", `String [%string "%{app_name}#%{device_type}"] ]
  in
  let body = Cohttp_async.Body.of_string (Jsonaf.to_string body_json) in
  let headers = Cohttp.Header.of_list [ "Content-Type", "application/json" ] in
  let%bind resp, resp_body = Cohttp_async.Client.post ~ssl_config ~headers ~body uri in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string resp_body in
  if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status))
  then
    Deferred.Or_error.error_s
      [%message "Auth HTTP %d: %s" (Cohttp.Code.code_of_status status : int) body_str]
  else (
    let%bind.Deferred.Or_error json = Jsonaf.parse body_str |> Deferred.return in
    match Jsonaf.list json with
    | None -> Deferred.Or_error.error_s [%message "Failed to parse auth response"]
    | Some [] | Some (_ :: _ :: _) ->
      Deferred.Or_error.error_s
        [%message "Unexpected response format" (body_str : string)]
    | Some [ entry ] ->
      (match Jsonaf.member "success" entry with
       | Some success ->
         (match Jsonaf.member "username" success with
          | Some username -> Deferred.Or_error.return (Jsonaf.string_exn username)
          | None -> Deferred.Or_error.error_s [%message "No username in success response"])
       | None ->
         (match Jsonaf.member "error" entry with
          | None ->
            Deferred.Or_error.error_s [%message "Unexpected response" (body_str : string)]
          | Some err ->
            let desc =
              match Jsonaf.member "description" err with
              | Some d -> Jsonaf.string_exn d
              | None -> "unknown error"
            in
            Deferred.Or_error.error_s [%message "Bridge error" desc])))
;;
