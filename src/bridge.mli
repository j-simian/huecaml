open! Core
open! Async

(** Connects to the Hue discovery endpoint and returns a list of IPs for Hue Bridges on the local network. *)
val discover : unit -> string list Deferred.Or_error.t

(** Sends an authentication request to the Hue Bridge. Requires the user to press the link button, at which point the function will return a username. *)
val authenticate
  :  bridge_ip:string
  -> device_type:string
  -> app_name:string
  -> string Deferred.Or_error.t
