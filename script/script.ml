open StdLabels
open Abstract_fields

let rec any ~f = function
  | [] -> false
  | hd :: tl -> if f hd then true else any ~f tl
;;

let rec all ~f = function
  | [] -> true
  | hd :: tl -> if f hd then all ~f tl else false
;;

let has_hebrew_script titles = any ~f:(fun t -> t.Title.script = "Hebr") titles

let add_if_exits (name, field) l =
  match field with [] -> l | _ -> (name, `List field) :: l

let json_of_record r : Yojson.t =
  let open Picaplus in
  let titles = Record.to_titles r
               |> List.map ~f:(fun t -> `String (Title.repr t)) in
  let series = Record.to_series r
               |> List.map ~f:(fun t -> `String (Title.repr t)) in
  let people = Record.to_creator_ppl r
               |> List.map ~f:(fun p ->
                      let p = person_cleanup p in
                      `String (Person.comma_name p)) in
  let years = Record.to_years r
              |> List.map ~f:(fun f -> `Int f) in
  let id = [`String (Record.get_ppn r)] in
  `Assoc (List.fold_right ~f:add_if_exits ~init:[]
            [ ("title", titles)
            ; ("isPartOf", series)
            ; ("creator", people)
            ; ("date", years)
            ; ("identifier", id)
    ])

let should_convert r =
  let open Picaplus in
  match Record.to_titles r, Record.to_series r with
  | [], [] -> false
  | [], series -> not (has_hebrew_script series)
  | titles, series -> not (has_hebrew_script titles || has_hebrew_script series)

let () =
  let records = Pica_io.records_of_ic ~sub_sep:(Re.Perl.compile_pat "ƒ") stdin in
  Seq.iter (fun r ->
      if should_convert r then
        ((json_of_record r |> Yojson.to_channel stdout);
         print_string "\n")
      else ()
    ) records
