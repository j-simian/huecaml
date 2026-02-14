open! Core
open! Async

val discover : unit -> string list Deferred.Or_error.t
val authenticate : bridge_ip:string -> device_type:string -> app_name:string -> string Deferred.Or_error.t
