open! Core
open! Async

type t =
  { id : Resource.Id.t
  ; metadata : Resource.Metadata.t
  ; children : Resource.Resource_ref.t list
  ; grouped_light : Resource.Resource_ref.t option
  }
[@@deriving sexp]

let parse_resource_ref json =
  { Resource.Resource_ref.rid = Jsonaf.string_exn (Jsonaf.member_exn "rid" json)
  ; rtype = Jsonaf.string_exn (Jsonaf.member_exn "rtype" json)
  }
;;

let parse_room json =
  let id = Jsonaf.string_exn (Jsonaf.member_exn "id" json) in
  let metadata_json = Jsonaf.member_exn "metadata" json in
  let metadata =
    { Resource.Metadata.name = Jsonaf.string_exn (Jsonaf.member_exn "name" metadata_json)
    ; archetype =
        (match Jsonaf.member "archetype" metadata_json with
         | Some a -> Some (Jsonaf.string_exn a)
         | None -> None)
    }
  in
  let children =
    match Jsonaf.member "children" json with
    | Some children_json -> List.map (Jsonaf.list_exn children_json) ~f:parse_resource_ref
    | None -> []
  in
  let grouped_light =
    match Jsonaf.member "services" json with
    | Some services ->
      List.find_map (Jsonaf.list_exn services) ~f:(fun svc ->
        let rtype = Jsonaf.string_exn (Jsonaf.member_exn "rtype" svc) in
        if String.equal rtype "grouped_light"
        then Some (parse_resource_ref svc)
        else None)
    | None -> None
  in
  { id; metadata; children; grouped_light }
;;

let get_all client =
  let%bind.Deferred.Or_error json = Client.get client "/room" in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    let rooms = List.map data ~f:parse_room in
    Deferred.Or_error.return rooms
  with
  | exn -> Deferred.Or_error.error_s [%message "Failed to parse rooms" (exn : exn)]
;;

let get client id =
  let%bind.Deferred.Or_error json = Client.get client [%string "/room/%{id}"] in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    match data with
    | [ room_json ] -> Deferred.Or_error.return (parse_room room_json)
    | _ -> Deferred.Or_error.error_s [%message "Expected exactly one room in response"]
  with
  | exn -> Deferred.Or_error.error_s [%message "Failed to parse room" (exn : exn)]
;;
