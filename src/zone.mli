open! Core
open! Async

type t =
  { id : Resource.Id.t
  ; metadata : Resource.Metadata.t
  ; children : Resource.Resource_ref.t list
  ; grouped_light : Resource.Resource_ref.t option
  }
[@@deriving sexp]

val get_all : Client.t -> t list Deferred.Or_error.t
val get : Client.t -> Resource.Id.t -> t Deferred.Or_error.t
