open! Core
open! Async

type t =
  { id : Resource.Id.t
  ; metadata : Resource.Metadata.t
  ; group : Resource.Resource_ref.t
  ; actions : Jsonaf.t list
  }
[@@deriving sexp]

val get_all : Client.t -> t list Deferred.Or_error.t
val get : Client.t -> Resource.Id.t -> t Deferred.Or_error.t
val recall : Client.t -> Resource.Id.t -> unit Deferred.Or_error.t
