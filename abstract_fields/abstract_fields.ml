open StdLabels
let ws = Re.Perl.compile_pat "^\\s*(.*)\\s*$"
let strip_whitespace s = Re.replace ws ~f:(fun g -> Re.Group.get g 1) s
let opt_to_null ~f = function
  | None -> `Null
  | Some v -> f v
let json_string (s: string) : Yojson.t = `String s

module Title = struct
  type t = { main: string
           ; sub: string option
           ; name: string option
           ; script: string
           }
  ;;
  let make ~main ~sub ~name ~script = {main; sub; name; script}

  let repr tl sep = function
    | None -> tl
    | Some s -> sep :: s :: tl
  let repr {main; sub; name; _} =
    let tl = repr [] "/" name in
    let tl = repr tl ":" sub in
    String.concat ~sep:" " (main :: tl)

  let to_yojson {main; sub; name; script} : Yojson.t =
    `Assoc
      [ ("main", `String main)
      ; ("sub", opt_to_null sub ~f:json_string)
      ; ("name", opt_to_null name ~f:json_string)
      ; ("script", `String script)
      ]
end

module Person = struct
  type t = { name: string
           ; first_name: string option
           ; identifiers: string list
           ; script: string
           }
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
       match String.split_on_char s ~sep:',' |> List.map ~f:strip_whitespace with
       | [name; fn] -> Ok {t with name; first_name = Some fn}
       | _ -> Error t
  let to_yojson {name; first_name; identifiers; script} : Yojson.t =
    `Assoc
      [ ("name", `String name)
      ; ("first_name", opt_to_null first_name ~f:json_string)
      ; ("identifiers", `List (List.map ~f:json_string identifiers))
      ; ("script", `String script)
      ]
end

module Publisher = struct (* I'm just a stub *) end
