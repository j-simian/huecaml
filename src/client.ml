open! Core
open! Async
open Cohttp
open Cohttp_async

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
  Uri.make ~scheme:"https" ~host:t.config.bridge_ip ~path:("/clip/v2/resource" ^ path) ()
;;

let headers t =
  Header.of_list
    [ "hue-application-key", t.config.app_key; "Content-Type", "application/json" ]
;;

let parse_response (resp, body) =
  let status = Response.status resp in
  let%bind body_str = Body.to_string body in
  if Code.is_success (Code.code_of_status status)
  then (
    match Jsonaf.parse body_str with
    | Ok json -> Deferred.Or_error.return json
    | Error err -> Deferred.return (Error err))
  else
    Deferred.Or_error.error_string
      (sprintf "HTTP %d: %s" (Code.code_of_status status) body_str)
;;

let get t path =
  let uri = base_uri t path in
  let headers = headers t in
  let%bind result = Client.get ~ssl_config ~headers uri in
  parse_response result
;;

let put t path body =
  let uri = base_uri t path in
  let headers = headers t in
  let body_str = Jsonaf.to_string body in
  let body = Body.of_string body_str in
  let%bind result = Client.put ~ssl_config ~headers ~body uri in
  parse_response result
;;

let post t path body =
  let uri = base_uri t path in
  let headers = headers t in
  let body_str = Jsonaf.to_string body in
  let body = Body.of_string body_str in
  let%bind result = Client.post ~ssl_config ~headers ~body uri in
  parse_response result
;;

let delete t path =
  let uri = base_uri t path in
  let headers = headers t in
  let%bind result = Client.delete ~ssl_config ~headers uri in
  parse_response result
;;
