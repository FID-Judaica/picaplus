open StdLabels
open Abstract_fields

let has_hebrew_script titles =
  match List.filter titles ~f:(fun t -> t.Title.script = "Hebr") with
  | [] -> false
  | _ -> true

let json_of_record r : Yojson.t =
  let open Picaplus in
  let titles = Record.to_titles r
               |> List.map ~f:(fun t -> `String (Title.repr t)) in
  let people = Record.to_creator_ppl r
               |> List.map ~f:(fun p ->
                      let p = person_cleanup p in
                      `String (Person.comma_name p)) in
  let years = Record.to_years r
              |> List.map ~f:(fun f -> `Int f) in
  let id = [`String (Record.get_ppn r)] in
  `Assoc
    [ ("title", `List titles)
    ; ("creator", `List people)
    ; ("date", `List years)
    ; ("identifier", `List id)
    ]

let () =
  let records = Pica_io.records_of_ic ~sub_sep:(Re.Perl.compile_pat "ƒ") stdin in
  Seq.iter (fun r ->
      let titles = Picaplus.Record.to_titles r in
      if has_hebrew_script titles || titles = [] then ()
      else
        ((json_of_record r |> Yojson.to_channel stdout);
         print_string "\n")
    ) records
