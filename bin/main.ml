open! Core
open! Async
open Deferred.Or_error.Let_syntax

let read_line () =
  let%map.Deferred result = Reader.read_line (Lazy.force Reader.stdin) in
  match result with
  | `Ok line -> Ok (Some line)
  | `Eof -> Ok None
;;

let discover_and_select_bridge () =
  printf "Discovering Hue bridges...\n%!";
  let%bind bridges = Huecaml.Bridge.discover () in
  match bridges with
  | [] -> Deferred.Or_error.error_s [%message "No bridges found"]
  | bridges ->
    List.iteri bridges ~f:(fun i ip -> printf "  [%d] %s\n" i ip);
    (match bridges with
     | [ ip ] ->
       printf "Auto-selecting the only bridge: %s\n%!" ip;
       return ip
     | _ ->
       printf "Select a bridge [0-%d]: %!" (List.length bridges - 1);
       let%map line = read_line () in
       let idx =
         match line with
         | Some s -> Int.of_string s
         | None -> 0
       in
       List.nth_exn bridges idx)
;;

let pair_with_bridge ~bridge_ip =
  print_endline "Press the link button on your Hue bridge, then press Enter.";
  printf "> %!";
  let%bind _line = read_line () in
  let rec attempt n =
    let%bind.Deferred result =
      Huecaml.Bridge.authenticate ~bridge_ip ~device_type:"cli" ~app_name:"huecaml"
    in
    match result with
    | Ok app_key ->
      printf "Paired successfully! App key: %s\n%!" app_key;
      return app_key
    | Error err ->
      if n <= 0
      then Deferred.Or_error.fail err
      else (
        printf "Waiting for link button press... (retries left: %d)\n%!" n;
        let%bind.Deferred () = Clock.after (Time_float.Span.of_sec 2.0) in
        attempt (n - 1))
  in
  attempt 15
;;

let list_lights client =
  let%map lights = Huecaml.Light.get_all client in
  print_endline "\n=== Lights ===";
  List.iter lights ~f:(fun (light : Huecaml.Light.t) ->
    let on_off = if light.on then "ON" else "OFF" in
    let brightness =
      match light.brightness with
      | Some b -> sprintf " brightness=%.0f%%" b
      | None -> ""
    in
    let color =
      match light.color with
      | Some c -> sprintf " color=(%.4f, %.4f)" c.x c.y
      | None -> ""
    in
    printf "  %s [%s]%s%s\n" light.metadata.name on_off brightness color)
;;

let list_rooms client =
  let%map rooms = Huecaml.Room.get_all client in
  print_endline "\n=== Rooms ===";
  List.iter rooms ~f:(fun (room : Huecaml.Room.t) ->
    printf "  %s (%d children)\n" room.metadata.name (List.length room.children))
;;

let list_scenes client =
  let%map scenes = Huecaml.Scene.get_all client in
  print_endline "\n=== Scenes ===";
  List.iter scenes ~f:(fun (scene : Huecaml.Scene.t) ->
    printf "  %s (group: %s/%s)\n" scene.metadata.name scene.group.rtype scene.group.rid)
;;

let main () =
  let%bind bridge_ip = discover_and_select_bridge () in
  let%bind app_key = pair_with_bridge ~bridge_ip in
  let client = Huecaml.Client.create { bridge_ip; app_key } in
  let%bind () = list_lights client in
  let%bind () = list_rooms client in
  let%map () = list_scenes client in
  ()
;;

let command =
  Command.async_or_error
    ~summary:"Interactive Hue bridge CLI"
    [%map_open.Command let () = return () in fun () -> main ()]
;;

let () = Command_unix.run command
