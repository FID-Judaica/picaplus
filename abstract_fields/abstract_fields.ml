open Base

module Title = struct
  type t = {
      main: string;
      sub: string option;
      name: string option;
      script: string;
    }
  ;;
  let make ~main ~sub ~name ~script = {main; sub; name; script}

  let repr tl sep = function
    | None -> tl
    | Some s -> sep :: s :: tl
  let repr {main; sub; name; _} =
    let tl = repr [] "/" name in
    let tl = repr tl ":" sub in
    String.concat(main :: tl)
end

module Person = struct
  type t = { name: string;
             first_name: string option;
             identifiers: string list;
             script: string;
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
end

module Publisher = struct (* I'm just a stub *) end
