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
