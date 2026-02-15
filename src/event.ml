open! Core
open! Async

type event_type =
  | Update
  | Add
  | Delete
[@@deriving sexp]

type resource_change =
  { id : Resource.Id.t
  ; rtype : string
  ; raw : Jsonaf.t
  }
[@@deriving sexp]

type t =
  { creation_time : string
  ; id : string
  ; event_type : event_type
  ; data : resource_change list
  }
[@@deriving sexp]

let event_type_of_string = function
  | "update" -> Ok Update
  | "add" -> Ok Add
  | "delete" -> Ok Delete
  | other -> Or_error.error_s [%message "Unknown event type" (other : string)]
;;

let parse_resource_change json =
  let id = Jsonaf.string_exn (Jsonaf.member_exn "id" json) in
  let rtype = Jsonaf.string_exn (Jsonaf.member_exn "type" json) in
  { id; rtype; raw = json }
;;

let parse_event json =
  let open Or_error.Let_syntax in
  let creation_time = Jsonaf.string_exn (Jsonaf.member_exn "creationtime" json) in
  let id = Jsonaf.string_exn (Jsonaf.member_exn "id" json) in
  let event_type_string = Jsonaf.string_exn (Jsonaf.member_exn "type" json) in
  let%map event_type = event_type_of_string event_type_string in
  let data =
    Jsonaf.member_exn "data" json
    |> Jsonaf.list_exn
    |> List.map ~f:parse_resource_change
  in
  { creation_time; id; event_type; data }
;;

let parse_sse_data_lines message =
  String.split_lines message
  |> List.filter_map ~f:(fun line ->
    match String.chop_prefix line ~prefix:"data: " with
    | Some data -> Some data
    | None ->
      (match String.chop_prefix line ~prefix:"data:" with
       | Some data -> Some (String.lstrip data)
       | None -> None))
  |> String.concat
;;

let parse_sse_message message =
  let data_string = parse_sse_data_lines message in
  match String.is_empty data_string with
  | true -> Ok []
  | false ->
    let open Or_error.Let_syntax in
    let%bind json = Jsonaf.parse data_string in
    let event_jsons = Jsonaf.list_exn json in
    Or_error.combine_errors (List.map event_jsons ~f:parse_event)
;;

let split_on_double_newline str =
  let rec split acc current remaining =
    match String.substr_index remaining ~pattern:"\n\n" with
    | None -> List.rev acc, current ^ remaining
    | Some idx ->
      let complete_message = current ^ String.prefix remaining idx in
      let rest = String.drop_prefix remaining (idx + 2) in
      split (complete_message :: acc) "" rest
  in
  split [] "" str
;;

let subscribe client =
  let open Deferred.Or_error.Let_syntax in
  let%map response_body = Client.subscribe_sse client in
  let raw_pipe = Cohttp_async.Body.to_pipe response_body in
  let reader, writer = Pipe.create () in
  don't_wait_for
    (let buffer = Buffer.create 4096 in
     let%bind.Deferred () =
       Pipe.iter raw_pipe ~f:(fun chunk ->
         Buffer.add_string buffer chunk;
         let contents = Buffer.contents buffer in
         let normalized =
           String.substr_replace_all contents ~pattern:"\r\n" ~with_:"\n"
         in
         let complete_messages, leftover = split_on_double_newline normalized in
         Buffer.clear buffer;
         Buffer.add_string buffer leftover;
         Deferred.List.iter ~how:`Sequential complete_messages ~f:(fun message ->
           match parse_sse_message message with
           | Ok events ->
             Deferred.List.iter ~how:`Sequential events ~f:(fun event ->
               Pipe.write_if_open writer event)
           | Error err ->
             [%log.global.debug
               "Failed to parse SSE message" (err : Error.t) (message : string)];
             Deferred.unit))
     in
     Pipe.close writer;
     Deferred.unit);
  reader
;;
