open! Core
open! Async
open Deferred.Or_error.Let_syntax

let config_dir =
  let home = Sys_unix.home_directory () in
  home ^/ ".config" ^/ "huecaml"
;;

let config_path = config_dir ^/ "config.json"

let read_line () =
  let%map.Deferred result = Reader.read_line (Lazy.force Reader.stdin) in
  match result with
  | `Ok line -> Ok (Some line)
  | `Eof -> Ok None
;;

let load_config () =
  match%map.Deferred Sys.file_exists config_path with
  | `Yes ->
    let contents = In_channel.read_all config_path in
    let json = Jsonaf.of_string contents in
    let bridge_ip = Jsonaf.member_exn "bridge_ip" json |> Jsonaf.string_exn in
    let app_key = Jsonaf.member_exn "app_key" json |> Jsonaf.string_exn in
    Ok (Some (bridge_ip, app_key))
  | `No | `Unknown -> Ok None
;;

let save_config ~bridge_ip ~app_key =
  let%map.Deferred () = Unix.mkdir ~p:() config_dir in
  let json = `Object [ "bridge_ip", `String bridge_ip; "app_key", `String app_key ] in
  Out_channel.write_all config_path ~data:(Jsonaf.to_string_hum json);
  Ok ()
;;

let discover_and_select_bridge () =
  print_endline "Discovering Hue bridges...";
  let%bind bridges = Huecaml.Bridge.discover () in
  match bridges with
  | [] -> Deferred.Or_error.error_s [%message "No bridges found"]
  | bridges ->
    List.iteri bridges ~f:(fun i ip ->
      let index = Int.to_string i in
      print_endline [%string "  [%{index}] %{ip}"]);
    (match bridges with
     | [ ip ] ->
       print_endline [%string "Auto-selecting the only bridge: %{ip}"];
       return ip
     | _ ->
       let last_index = Int.to_string (List.length bridges - 1) in
       print_string [%string "Select a bridge [0-%{last_index}]: "];
       Out_channel.flush Out_channel.stdout;
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
  print_string "> ";
  Out_channel.flush Out_channel.stdout;
  let%bind _line = read_line () in
  let rec attempt retries_remaining =
    let%bind.Deferred result =
      Huecaml.Bridge.authenticate ~bridge_ip ~device_type:"cli" ~app_name:"huecaml"
    in
    match result with
    | Ok app_key ->
      print_endline [%string "Paired successfully! App key: %{app_key}"];
      return app_key
    | Error err ->
      if retries_remaining <= 0
      then Deferred.Or_error.fail err
      else (
        let retries = Int.to_string retries_remaining in
        print_endline [%string "Waiting for link button press... (retries left: %{retries})"];
        let%bind.Deferred () = Clock.after (Time_float.Span.of_sec 2.0) in
        attempt (retries_remaining - 1))
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
      | Some brightness ->
        let brightness_percentage =
          Float.to_string_hum ~decimals:0 (brightness *. 100.)
        in
        [%string " brightness=%{brightness_percentage}%%"]
      | None -> ""
    in
    let color =
      match light.color with
      | Some color ->
        let x = Float.to_string_hum ~decimals:4 color.x in
        let y = Float.to_string_hum ~decimals:4 color.y in
        [%string " color=(%{x}, %{y})"]
      | None -> ""
    in
    let name = light.metadata.name in
    print_endline [%string "  %{name} [%{on_off}]%{brightness}%{color}"])
;;

let list_rooms client =
  let%map rooms = Huecaml.Room.get_all client in
  print_endline "\n=== Rooms ===";
  List.iter rooms ~f:(fun (room : Huecaml.Room.t) ->
    let name = room.metadata.name in
    let children_count = Int.to_string (List.length room.children) in
    print_endline [%string "  %{name} (%{children_count} children)"])
;;

let list_scenes client =
  let%map scenes = Huecaml.Scene.get_all client in
  print_endline "\n=== Scenes ===";
  List.iter scenes ~f:(fun (scene : Huecaml.Scene.t) ->
    let name = scene.metadata.name in
    let group_rtype = scene.group.rtype in
    let group_rid = scene.group.rid in
    print_endline [%string "  %{name} (group: %{group_rtype}/%{group_rid})"])
;;

let get_or_create_credentials () =
  let%bind config = load_config () in
  match config with
  | Some (bridge_ip, app_key) ->
    print_endline [%string "Using saved credentials for bridge %{bridge_ip}"];
    return (bridge_ip, app_key)
  | None ->
    let%bind bridge_ip = discover_and_select_bridge () in
    let%bind app_key = pair_with_bridge ~bridge_ip in
    let%map () = save_config ~bridge_ip ~app_key in
    print_endline [%string "Credentials saved to %{config_path}"];
    bridge_ip, app_key
;;

let main () =
  let%bind bridge_ip, app_key = get_or_create_credentials () in
  let client = Huecaml.Client.create { bridge_ip; app_key } in
  let%bind () = list_lights client in
  let%bind () = list_rooms client in
  let%map () = list_scenes client in
  ()
;;

let command =
  Command.async_or_error
    ~summary:"Interactive Hue bridge CLI"
    [%map_open.Command
      let () = return () in
      fun () -> main ()]
;;

let () = Command_unix.run command
