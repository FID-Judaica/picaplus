open Base
let ws = Re.Perl.compile_pat "^\\s*(.*)\\s*$"
let strip_whitespace s = Re.replace ws ~f:(fun g -> Re.Group.get g 1) s

let rec any ~f = function
  | [] -> false
  | hd :: tl -> if f hd then true else any ~f tl
;;

module Title = struct
  type t = { main: string
           ; sub: string option
           ; name: string option
           ; script: string
           } [@@deriving sexp]
  ;;
  let make ~main ~sub ~name ~script = {main; sub; name; script}

  let repr tl sep = function
    | None -> tl
    | Some s -> sep :: s :: tl
  let repr {main; sub; name; _} =
    let tl = repr [] "/" name in
    let tl = repr tl ":" sub in
    String.concat ~sep:" " (main :: tl)
  ;;
  let has_hebrew_script titles =
    any ~f:(fun t -> String.equal t.script "Hebr") titles
end

module Person = struct
  type t = { name: string
           ; first_name: string option
           ; identifiers: string list
           ; script: string
           } [@@deriving sexp]
  ;;
  let make ?first_name ~name ~identifiers ~script =
    {name; first_name; identifiers; script}
  let or_name t ~f =
    match t.first_name with
    | None -> t.name
    | Some fn -> f fn t.name
  let natural_name =
    or_name ~f:(fun fn name -> String.concat ~sep:" " [fn; name])
  let comma_name =
    or_name ~f:(fun fn name -> String.concat ~sep:", " [name; fn])
  let comma_split t =
    match t.name, t.first_name with
    | _, Some _ -> Ok t
    | s, _ ->
       match String.split s ~on:',' |> List.map ~f:strip_whitespace with
       | [name; fn] -> Ok {t with name; first_name = Some fn}
       | _ -> Error t
end

module Publisher = struct
  type t = { name: string option
           ; place: string list
           ; script: string
           } [@@deriving sexp]
  let make ?name ~place ~script = {name; place; script}
end
