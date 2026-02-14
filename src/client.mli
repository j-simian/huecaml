open! Core
open! Async

module Config : sig
  type t =
    { bridge_ip : string
    ; app_key : string
    }
end

type t

val create : Config.t -> t
val get : t -> string -> Jsonaf.t Deferred.Or_error.t
val put : t -> string -> Jsonaf.t -> Jsonaf.t Deferred.Or_error.t
val post : t -> string -> Jsonaf.t -> Jsonaf.t Deferred.Or_error.t
val delete : t -> string -> Jsonaf.t Deferred.Or_error.t
