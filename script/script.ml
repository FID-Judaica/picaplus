open Abstract_fields

let should_convert r =
  let open Picaplus in
  match Record.to_titles r, Record.to_series r with
  | [], [] -> false
  | [], series -> not (Title.has_hebrew_script series)
  | titles, series -> not (Title.has_hebrew_script titles || Title.has_hebrew_script series)

let () =
  let open Picaplus in
  let records = Pica_io.records_of_ic ~sub_sep:(Re.Perl.compile_pat "ƒ") stdin in
  Seq.iter (fun r ->
      if should_convert r then
        ((Record.to_api_json ~person_cleanup r |> Yojson.to_channel stdout);
         print_string "\n")
      else ()
    ) records
