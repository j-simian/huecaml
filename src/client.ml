open! Core
open! Async

module Config = struct
  type t =
    { bridge_ip : string
    ; app_key : string
    }
end

type t = { config : Config.t }

let create config = { config }

let ssl_config =
  Conduit_async.V2.Ssl.Config.create ~verify_modes:[] ~verify:(fun _ -> return true) ()
;;

let base_uri t path =
  let bridge_ip = t.config.bridge_ip in
  Uri.make ~scheme:"https" ~host:bridge_ip ~path:[%string "/clip/v2/resource%{path}"] ()
;;

let headers t =
  Cohttp.Header.of_list
    [ "hue-application-key", t.config.app_key; "Content-Type", "application/json" ]
;;

let parse_response (response, response_body) =
  let response_status = Cohttp.Response.status response in
  let response_status_type =
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status response_status)
    then `Success
    else `Error
  in
  let%bind body_string = Cohttp_async.Body.to_string response_body in
  match response_status_type with
  | `Error ->
    let http_error_code = Cohttp.Code.code_of_status response_status in
    Deferred.Or_error.error_s
      [%message "HTTP error" (http_error_code : int) (body_string : string)]
  | `Success ->
    (match Jsonaf.parse body_string with
     | Ok json -> Deferred.Or_error.return json
     | Error err -> Deferred.return (Error err))
;;

let get t path =
  let uri = base_uri t path in
  let headers = headers t in
  let%bind result = Cohttp_async.Client.get ~ssl_config ~headers uri in
  parse_response result
;;

let put t path body =
  let uri = base_uri t path in
  let headers = headers t in
  let body_str = Jsonaf.to_string body in
  let body = Cohttp_async.Body.of_string body_str in
  let%bind result = Cohttp_async.Client.put ~ssl_config ~headers ~body uri in
  parse_response result
;;

let post t path body =
  let uri = base_uri t path in
  let headers = headers t in
  let body_str = Jsonaf.to_string body in
  let body = Cohttp_async.Body.of_string body_str in
  let%bind result = Cohttp_async.Client.post ~ssl_config ~headers ~body uri in
  parse_response result
;;

let delete t path =
  let uri = base_uri t path in
  let headers = headers t in
  let%bind result = Cohttp_async.Client.delete ~ssl_config ~headers uri in
  parse_response result
;;
