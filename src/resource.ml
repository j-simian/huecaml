open! Core

module Id = struct
  type t = string [@@deriving sexp, compare, equal]

  let jsonaf_of_t s = `String s
  let t_of_jsonaf json = Jsonaf.string_exn json
end

module Metadata = struct
  type t =
    { name : string
    ; archetype : string option
    }
  [@@deriving sexp]

  let jsonaf_of_t t =
    let fields = [ "name", `String t.name ] in
    let fields =
      match t.archetype with
      | Some a -> fields @ [ "archetype", `String a ]
      | None -> fields
    in
    `Object fields
  ;;

  let t_of_jsonaf json =
    { name = Jsonaf.string_exn (Jsonaf.member_exn "name" json)
    ; archetype = Option.map ~f:Jsonaf.string_exn (Jsonaf.member "archetype" json)
    }
  ;;
end

module Resource_ref = struct
  type t =
    { rid : Id.t
    ; rtype : string
    }
  [@@deriving sexp]

  let jsonaf_of_t t =
    `Object [ "rid", `String t.rid; "rtype", `String t.rtype ]
  ;;

  let t_of_jsonaf json =
    { rid = Jsonaf.string_exn (Jsonaf.member_exn "rid" json)
    ; rtype = Jsonaf.string_exn (Jsonaf.member_exn "rtype" json)
    }
  ;;
end
