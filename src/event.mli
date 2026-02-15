open! Core
open! Async

type event_type =
  | Update
  | Add
  | Delete
[@@deriving sexp]

type resource_change =
  { id : Resource.Id.t
  ; rtype : string
  ; raw : Jsonaf.t
  }
[@@deriving sexp]

type t =
  { creation_time : string
  ; id : string
  ; event_type : event_type
  ; data : resource_change list
  }
[@@deriving sexp]

val subscribe : Client.t -> t Pipe.Reader.t Deferred.Or_error.t
