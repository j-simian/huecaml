open! Core

let parse_exn s =
  match Jsonaf.parse s with
  | Ok json -> json
  | Error err -> raise_s [%sexp (err : Error.t)]
;;

let%expect_test "Resource.Id roundtrips through JSON" =
  let id : Huecaml.Resource.Id.t = "abc-123-def" in
  let json = Huecaml.Resource.Id.jsonaf_of_t id in
  let str = Jsonaf.to_string json in
  print_endline str;
  let id' = Huecaml.Resource.Id.t_of_jsonaf (parse_exn str) in
  print_endline id';
  [%expect {|
    "abc-123-def"
    abc-123-def |}]
;;

let%expect_test "Resource.Metadata roundtrips through JSON" =
  let meta : Huecaml.Resource.Metadata.t =
    { name = "Living Room"; archetype = Some "room" }
  in
  let json = Huecaml.Resource.Metadata.jsonaf_of_t meta in
  let str = Jsonaf.to_string json in
  print_endline str;
  let meta' = Huecaml.Resource.Metadata.t_of_jsonaf (parse_exn str) in
  print_s [%sexp (meta' : Huecaml.Resource.Metadata.t)];
  [%expect {|
    {"name":"Living Room","archetype":"room"}
    ((name "Living Room") (archetype (room))) |}]
;;

let%expect_test "Resource.Metadata without archetype" =
  let meta : Huecaml.Resource.Metadata.t = { name = "Test"; archetype = None } in
  let json = Huecaml.Resource.Metadata.jsonaf_of_t meta in
  let str = Jsonaf.to_string json in
  print_endline str;
  [%expect {| {"name":"Test"} |}]
;;

let%expect_test "Resource.Resource_ref roundtrips through JSON" =
  let ref_ : Huecaml.Resource.Resource_ref.t =
    { rid = "some-uuid"; rtype = "light" }
  in
  let json = Huecaml.Resource.Resource_ref.jsonaf_of_t ref_ in
  let str = Jsonaf.to_string json in
  print_endline str;
  let ref' = Huecaml.Resource.Resource_ref.t_of_jsonaf (parse_exn str) in
  print_s [%sexp (ref' : Huecaml.Resource.Resource_ref.t)];
  [%expect {|
    {"rid":"some-uuid","rtype":"light"}
    ((rid some-uuid) (rtype light)) |}]
;;

let%expect_test "Light.Color roundtrips through JSON" =
  let color : Huecaml.Light.Color.t = { x = 0.3; y = 0.5 } in
  let json = Huecaml.Light.Color.jsonaf_of_t color in
  let str = Jsonaf.to_string json in
  print_endline str;
  let color' = Huecaml.Light.Color.t_of_jsonaf (parse_exn str) in
  print_s [%sexp (color' : Huecaml.Light.Color.t)];
  [%expect {|
    {"x":0.3,"y":0.5}
    ((x 0.3) (y 0.5))
    |}]
;;

let%expect_test "Client.create returns a client" =
  let _client =
    Huecaml.Client.create { bridge_ip = "192.168.1.100"; app_key = "test-key" }
  in
  print_endline "client created";
  [%expect {| client created |}]
;;
