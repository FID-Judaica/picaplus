open Base

let ws = Re.Perl.compile_pat "^(\\s*)(.*)(\\s*$)"
let strip_whitespace s = Re.replace ws ~f:(fun g -> Re.Group.get g 2) s

let rec any ~f = function
  | [] -> false
  | hd :: tl -> if f hd then true else any ~f tl
;;
module Nonfiling : sig
  type t = (string * string) option
  [@@deriving sexp]
  val make : string -> string -> t
  val destructure : t -> (string * string) option
  val none : t
  type joiner =
    nonfiling:string -> whitespace:string -> main:string -> string
  val join : main:string -> nonfiling:t -> f:joiner -> string

end = struct
  type t = (string * string) option
  [@@deriving sexp]
  let make s1 s2 = Some (s1, s2)
  let destructure t = t
  let none : t = None
  type joiner =
    nonfiling:string -> whitespace:string -> main:string -> string
  ;;
  let join ~main ~nonfiling ~f =
    match nonfiling with
    | None -> main
    | Some (nonfiling, whitespace) ->
      f ~nonfiling ~whitespace ~main
  ;;
end


module Title = struct
  type t =
    { main : string
    ; nonfiling: Nonfiling.t
    ; sub : string option
    ; name : string option
    ; script : string
    }
  [@@deriving sexp]

  let make ~main ~nonfiling ~sub ~name ~script =
    { main; nonfiling; sub; name; script }
  ;;
  let _add_repr_field tl sep = function
    | None -> tl
    | Some s -> sep :: s :: tl
  ;;
  let _default_joiner ~nonfiling ~whitespace ~main =
    String.concat ["{"; nonfiling; "}"; whitespace; main]
      
  let repr ?(joiner = _default_joiner) { main; nonfiling; sub; name; _ } =
    let main = Nonfiling.join ~main ~nonfiling ~f:joiner in
    let tl = _add_repr_field [] "/" name in
    let tl = _add_repr_field tl ":" sub in
    String.concat ~sep:" " (main :: tl)
  ;;

  let has_hebrew_script titles = any ~f:(fun t -> String.equal t.script "Hebr") titles
end

module Person = struct
  type t =
    { name : string
    ; first_name : string option
    ; identifiers : string list
    ; script : string
    }
  [@@deriving sexp]

  let make ?first_name ~name ~identifiers ~script =
    { name; first_name; identifiers; script }
  ;;

  let or_name t ~f =
    match t.first_name with
    | None -> t.name
    | Some fn -> f fn t.name
  ;;

  let natural_name = or_name ~f:(fun fn name -> String.concat ~sep:" " [ fn; name ])
  let comma_name = or_name ~f:(fun fn name -> String.concat ~sep:", " [ name; fn ])

  let comma_split t =
    match t.name, t.first_name with
    | _, Some _ -> Ok t
    | s, _ ->
      (match String.split s ~on:',' |> List.map ~f:strip_whitespace with
      | [ name; fn ] -> Ok { t with name; first_name = Some fn }
      | _ -> Error t)
  ;;
end

module Publisher = struct
  type t =
    { name : string option
    ; place : string list
    ; script : string
    }
  [@@deriving sexp]

  let make ?name ~place ~script = { name; place; script }
end
