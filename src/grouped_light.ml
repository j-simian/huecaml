open! Core
open! Async

type t =
  { id : Resource.Id.t
  ; owner : Resource.Resource_ref.t
  ; on : bool
  ; brightness : float option
  ; color : Light.Color.t option
  ; color_temperature : int option
  }
[@@deriving sexp]

let parse_grouped_light json =
  let id = Jsonaf.string_exn (Jsonaf.member_exn "id" json) in
  let owner_json = Jsonaf.member_exn "owner" json in
  let owner =
    { Resource.Resource_ref.rid = Jsonaf.string_exn (Jsonaf.member_exn "rid" owner_json)
    ; rtype = Jsonaf.string_exn (Jsonaf.member_exn "rtype" owner_json)
    }
  in
  let on =
    let on_obj = Jsonaf.member_exn "on" json in
    Jsonaf.bool_exn (Jsonaf.member_exn "on" on_obj)
  in
  let brightness =
    match Jsonaf.member "dimming" json with
    | Some dimming ->
      (match Jsonaf.member "brightness" dimming with
       | Some b -> Some (Jsonaf.float_exn b)
       | None -> None)
    | None -> None
  in
  let color =
    match Jsonaf.member "color" json with
    | Some color_obj ->
      (match Jsonaf.member "xy" color_obj with
       | Some xy ->
         let x = Jsonaf.float_exn (Jsonaf.member_exn "x" xy) in
         let y = Jsonaf.float_exn (Jsonaf.member_exn "y" xy) in
         Some { Light.Color.x; y }
       | None -> None)
    | None -> None
  in
  let color_temperature =
    match Jsonaf.member "color_temperature" json with
    | Some ct ->
      (match Jsonaf.member "mirek" ct with
       | Some `Null -> None
       | Some m -> Some (Jsonaf.int_exn m)
       | _ -> None)
    | None -> None
  in
  { id; owner; on; brightness; color; color_temperature }
;;

let get_all client =
  let%bind.Deferred.Or_error json = Client.get client "/grouped_light" in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    let grouped_lights = List.map data ~f:parse_grouped_light in
    Deferred.Or_error.return grouped_lights
  with
  | exn ->
    Deferred.Or_error.error_s [%message "Failed to parse grouped lights" (exn : exn)]
;;

let get client id =
  let%bind.Deferred.Or_error json =
    Client.get client [%string "/grouped_light/%{id}"]
  in
  try
    let data = Jsonaf.list_exn (Jsonaf.member_exn "data" json) in
    match data with
    | [ grouped_light_json ] ->
      Deferred.Or_error.return (parse_grouped_light grouped_light_json)
    | _ ->
      Deferred.Or_error.error_s
        [%message "Expected exactly one grouped light in response"]
  with
  | exn ->
    Deferred.Or_error.error_s [%message "Failed to parse grouped light" (exn : exn)]
;;

let set_on client id on =
  let body = `Object [ "on", `Object [ ("on", if on then `True else `False) ] ] in
  let%bind.Deferred.Or_error _json =
    Client.put client [%string "/grouped_light/%{id}"] body
  in
  Deferred.Or_error.return ()
;;

let set_brightness client id brightness =
  let body =
    `Object [ "dimming", `Object [ "brightness", `Number (Float.to_string brightness) ] ]
  in
  let%bind.Deferred.Or_error _json =
    Client.put client [%string "/grouped_light/%{id}"] body
  in
  Deferred.Or_error.return ()
;;

let set_color client id (color : Light.Color.t) =
  let body =
    `Object
      [ ( "color"
        , `Object
            [ ( "xy"
              , `Object
                  [ "x", `Number (Float.to_string color.x)
                  ; "y", `Number (Float.to_string color.y)
                  ] )
            ] )
      ]
  in
  let%bind.Deferred.Or_error _json =
    Client.put client [%string "/grouped_light/%{id}"] body
  in
  Deferred.Or_error.return ()
;;

let set_color_temperature client id mirek =
  let body =
    `Object [ "color_temperature", `Object [ "mirek", `Number (Int.to_string mirek) ] ]
  in
  let%bind.Deferred.Or_error _json =
    Client.put client [%string "/grouped_light/%{id}"] body
  in
  Deferred.Or_error.return ()
;;
