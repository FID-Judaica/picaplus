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
  let repr {main; sub; name; _} =
    let rec repr = function
        | `Main (main, sub, name) -> main :: repr (`Sub (sub, name))
        | `Sub (Some sub, name) -> " : " :: sub :: repr (`Name name)
        | `Sub (None, name) -> repr (`Name name)
        | `Name (Some name) -> " / " :: name :: []
        | `Name None -> [] in
    `Main (main, sub, name) |> repr |> String.concat
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
