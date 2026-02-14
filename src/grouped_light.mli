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

val get_all : Client.t -> t list Deferred.Or_error.t
val get : Client.t -> Resource.Id.t -> t Deferred.Or_error.t
val set_on : Client.t -> Resource.Id.t -> bool -> unit Deferred.Or_error.t
val set_brightness : Client.t -> Resource.Id.t -> float -> unit Deferred.Or_error.t
val set_color : Client.t -> Resource.Id.t -> Light.Color.t -> unit Deferred.Or_error.t
val set_color_temperature : Client.t -> Resource.Id.t -> int -> unit Deferred.Or_error.t
