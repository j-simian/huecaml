open! Core
open! Async

type t =
  { id : Resource.Id.t
  ; metadata : Resource.Metadata.t
  ; group : Resource.Resource_ref.t
  ; actions : Jsonaf.t list
  }
[@@deriving sexp]

let parse_scene json =
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
  let group_json = Jsonaf.member_exn "group" json in
  let group =
    { Resource.Resource_ref.rid = Jsonaf.string_exn (Jsonaf.member_exn "rid" group_json)
    ; rtype = Jsonaf.string_exn (Jsonaf.member_exn "rtype" group_json)
    }
  in
  let actions =
    match Jsonaf.member "actions" json with
    | Some actions_json -> Jsonaf.list_exn actions_json
    | None -> []
  in
  { id; metadata; group; actions }
;;

let get_all client =
  let%bind.Deferred.Or_error json = Client.get client "/scene" in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    let scenes = List.map data ~f:parse_scene in
    Deferred.Or_error.return scenes
  with
  | exn ->
    Deferred.Or_error.error_string
      (sprintf "Failed to parse scenes: %s" (Exn.to_string exn))
;;

let get client id =
  let%bind.Deferred.Or_error json = Client.get client (sprintf "/scene/%s" id) in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    match data with
    | [ scene_json ] -> Deferred.Or_error.return (parse_scene scene_json)
    | _ -> Deferred.Or_error.error_string "Expected exactly one scene in response"
  with
  | exn ->
    Deferred.Or_error.error_string
      (sprintf "Failed to parse scene: %s" (Exn.to_string exn))
;;

let recall client id =
  let body = `Object [ "recall", `Object [ "action", `String "active" ] ] in
  let%bind.Deferred.Or_error _json = Client.put client (sprintf "/scene/%s" id) body in
  Deferred.Or_error.return ()
;;
