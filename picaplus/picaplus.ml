open Base
open Result.Monad_infix
open Abstract_fields
open Printf

type 'a string_tbl = (string,'a) Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

let nonfiling_pat = Re.Perl.compile_pat "([^@]+?)(\\s*)@(.*)"

(* this needs to be reimplemented to handle unicode *)
let stripper_factory chars =
  let chars = Hash_set.of_list (module Char) chars in
  fun string ->
  let rec loop start stop step =
    if start = stop then stop
    else if Hash_set.mem chars string.[start] then start
    else loop (start+step) stop step in
  let len = String.length string in
  let front = loop 0 len 1 in
  let back = loop (len-1) (-1) (-1) + 1 in
  let sub pos len = String.sub string ~pos ~len in
  if front = len then ("", "", "") else
    (sub 0 front, sub front (back-front), sub (back) (len-back))

let num_strip = stripper_factory (String.to_list "0912345678")

let years_of_date_str str =
  List.filter_map
    (String.split_on_chars str ~on:[' '; '-'; '/'])
    ~f:(fun part ->
      let _, num, _ = num_strip part in
      Caml.int_of_string_opt num)

let unzip l=
  let out1, out2 =
    List.fold_left
      l ~init:([], [])
      ~f:(fun (acc1, acc2) (el1, el2) -> el1 :: acc1, el2 :: acc2) in
  List.rev out1, List.rev out2

module Subfields = struct
  type t = (char * string) list
  let to_list t : (char * string) list = t
  let of_list (l:(char * string) list) : t = l

  let of_string sub_sep fld_str : t =
    let subs = Re.split sub_sep fld_str in
    let kv s = s.[0], (String.sub s ~pos:1 ~len:((String.length s) - 1)) in
    List.map ~f:kv subs
  ;;
  let find (subs:t) ~tag = 
    let f (k, v) = if Char.(tag = k) then Some v else None in
    List.filter_map ~f subs
  ;;
  let find_one (subs:t) ~tag =
    match find subs ~tag with
    | [] -> Error `NoMatch
    | [x] -> Ok x
    | _ -> Error `MultiMatch
  ;;
  let get_script (subs:t) =
    match find_one subs ~tag:'U' with
    | Error _ -> "Latn"
    | Ok script -> script
  ;;
  let chop_parallel str =
    match String.lsplit2 str ~on:'=' with
    | None -> str
    | Some (str, _) -> str
  ;;
  let separate_nonfiling str =
    match Re.exec_opt nonfiling_pat str with
    | None -> (Nonfiling.none, str)
    | Some groups ->
      let get = Re.Group.get groups in
      Nonfiling.make (get 1) (get 2), get 3
  ;;
  let to_title ?label subs =
    let f main =
      let main' = chop_parallel main in
      let nonfiling, main'' = separate_nonfiling main' in
      let sub = find_one ~tag:'d' subs >>| chop_parallel |> Result.ok in
      let name = find_one ~tag:'h' subs |> Result.ok in
      let script = get_script subs in
      Title.make ~main:main'' ~nonfiling ~sub ~name ~script ?field:label in
    find_one ~tag:'a' subs >>| f
  ;;
  let to_publisher ?label subs =
    let script = get_script subs in
    match find_one subs ~tag:'n', find subs ~tag:'p' with
    | Error _, [] -> Error `NoMatch
    | Error _, place -> Ok (Publisher.make ?name:None ~place ~script ?field:label)
    | Ok name, place -> Ok (Publisher.make ~name ~place ~script ?field:label)
  ;;                    
  let to_person_ppn subs = find_one ~tag:'9' subs
  let to_person ?label subs =
    let get sf = find_one ~tag:sf subs in
    let identifiers = to_person_ppn subs |> Caml.Result.to_list in
    let partial = Person.make ?field:label ~script:(get_script subs) ~identifiers in
    match get 'a', get 'd' with
    | Ok name, fn -> Some (partial ?first_name:(Result.ok fn) ~name)
    | Error _, Ok name -> Some (partial ?first_name:None ~name)
    | Error _, Error _ ->
       let rec loop = function
         | [] -> None
         | fld :: tl ->
            match get fld with
            | Ok name -> Some (partial ?first_name:None ~name)
            | _ -> loop tl in
       loop ['8'; 'P'; '9']
  ;; 
end

module Fields = struct
  type t = { data: string list
           ; sub_sep: Re.re
           }
  ;;
  let make data sub_sep = {data; sub_sep}
  ;;
  let subs flds =
    List.map ~f:(Subfields.of_string flds.sub_sep) flds.data
  ;;
  let find_sequence fld ~tag =
    Sequence.of_list fld.data
    |> Sequence.map ~f:(Subfields.of_string fld.sub_sep)
    |> Sequence.map ~f:(fun subs -> Subfields.find subs ~tag, subs)
  ;;
  let find fld ~tag = find_sequence fld ~tag |> Sequence.to_list
  ;;
  let find_one fld ~tag =
    let not_empty (xs, _) = not Poly.(xs = []) in
    match Sequence.filter ~f:not_empty (find_sequence fld ~tag)
          |> Sequence.to_list
    with
    | [] -> Error `NoMatch
    | [[x], y] -> Ok (x, y)
    | _ -> Error `MultiMatch
  ;;
  let map ?label ~f flds =
    List.filter_map ~f:(fun sub -> Result.ok (f ?label sub)) (subs flds)
end

module Record = struct
  let creator_codes = ["028A"; "028@"; "028P"]
  type t = { ppn: string
           ; fields: string list string_tbl
           ; sub_sep: Re.re
           }
  ;;
  let make ppn fields sub_sep = {ppn; fields; sub_sep}
  ;;
  let of_lines lines ~ppn ~sub_sep =
    {
      ppn;
      sub_sep;
      fields = begin
        List.map ~f:(String.lsplit2_exn ~on:' ') lines
        |> Hashtbl.of_alist_multi (module String)
      end
    }
  ;;
  let fields record = record.fields     
  ;;
  let find record ~label =
    match Hashtbl.find record.fields label with
    | None -> Error `NoMatch
    | Some flds -> Ok (Fields.make flds record.sub_sep)
  ;;
  let find_one record ~label =
    find record ~label >>= fun fld ->
    match fld.data with
    | [x] -> Ok (Subfields.of_string record.sub_sep x)
    | _ -> Error `MultiMatch
  ;;
  let find_numbered record ~label =
    let rec loop n =
      let label = sprintf "%s/%0.2d" label n in
      match find record ~label with
      | Error _ -> []
      | Ok fields -> fields :: loop (n+1) in
    loop 1
  let find_one_sub record ~label ~tag =
    find_one record ~label >>= Subfields.find_one ~tag
  ;;
  let map ~f ~label record =
    match find record ~label with
    | Error _ -> []
    | Ok flds -> Fields.map ~label ~f flds
  ;;
  let to_titles ?(label="021A") =
    map ~f:Subfields.to_title ~label
  ;;
  let to_series record =
    List.concat_map ["036C"; "036C/00"; "036E/00"]
      ~f:(fun label -> to_titles ~label record)
  ;;
  let to_creator_ppl ?(sub_func=Subfields.to_person) record =
    let intellectual_creators =
      List.filter_map creator_codes
        ~f:(fun label ->
            find record ~label
            |> Result.map ~f:(fun r -> r, label) |> Result.ok) in
    let editors = find_numbered record ~label:"028B"
                  |> List.map ~f:(fun x -> x, "028B") in
    let others = match find record ~label:"028C" with
      | Error _ -> []
      | Ok flds -> [flds, "028C"] in
    List.concat_map
      [intellectual_creators; others; editors]
      ~f:(fun flds -> List.concat_map
             flds
             ~f:(fun (fld, label) ->
                 Fields.subs fld |> List.filter_map ~f:(fun s -> sub_func ~label s)))
  ;;
  let to_years record =
    match find record ~label:"011@" with
    | Error _ -> []
    | Ok flds ->
       List.concat_map (Fields.subs flds) ~f:(fun subs ->
           let subs = Subfields.to_list subs in
           List.concat_map subs ~f:(fun (_, date_str) ->
               years_of_date_str date_str))
  ;;
  let to_publisher record =
    List.concat_map ["033A"; "033C"]
      ~f:(fun label -> map ~f:Subfields.to_publisher ~label record)
  ;;
  let get_ppn record = record.ppn
    (* find_one_sub record ~label:"003@" ~tag:'0'
     * |> Result.map_error ~f:(fun _ -> Failure "no ppn")
     * |> Result.ok_exn *)
  ;;
  let add_if_exits (name, field) l =
    match field with [] -> l | _ -> (name, `List field) :: l
  ;;
  let to_api_json ?(person_cleanup=Fn.id) r : Yojson.t =
    let titles = to_titles r
                 |> List.map ~f:(fun t -> `String (Title.repr t)) in
    let series, series_fields =
      to_series r
      |> List.map ~f:(fun t -> `String (Title.repr t), `String (Title.field_or t ""))
      |> unzip
    in
    let people, people_fields =
      to_creator_ppl r
      |> List.map
        ~f:(fun p ->
            let p = person_cleanup p in
            `String (Person.comma_name p), `String (Person.field_or p ""))
      |> unzip
    in
    let years = to_years r
                |> List.map ~f:(fun y -> `Int y) in
    let publishers, publisher_fields =
      to_publisher r
      |> List.map ~f:(fun pub ->
             let words =
               match pub.Publisher.name with
               | None -> pub.place
               | Some name -> name :: pub.place in
             `String (String.concat ~sep:", " words), `String (Publisher.field_or pub "")
        )
      |> unzip
    in
    let id = [`String (get_ppn r)] in
    `Assoc (List.fold_right ~f:add_if_exits ~init:[]
              [ ("title", titles)
              ; ("isPartOf", series)
              ; ("_seriesFields", series_fields)
              ; ("creator", people)
              ; ("_creatorFields", people_fields)
              ; ("date", years)
              ; ("publisher", publishers)
              ; ("_publisherFields", publisher_fields)
              ; ("identifier", id)
              ]
           )
end


let gnd_pat = Re.Perl.compile_pat "(\\d+X?)([^*]*).*; ID: (.*)"

let get_gnd_name s =
  let open Option in
  let get = Re.Group.get in
  Re.exec_opt gnd_pat s >>= fun g ->
  let name = match get g 2 |> String.strip |> String.split ~on:'$' with
    | [] -> failwith "shouldn't be able to get here"
    | [n] -> n
    | head :: fields ->
       let subs = 
         let kv s = s.[0], (String.sub s ~pos:1 ~len:((String.length s) - 1)) in
         List.map ~f:kv fields |> Subfields.of_list in
        match Subfields.to_person subs with
        | None -> head
        | Some p -> Person.comma_name p in
  let ppn = get g 1 in
  let gnd = get g 3 in
  return (name, [ppn; gnd])

let convert_gnd_person p =
  match get_gnd_name p.Person.name with
  | None -> p
  | Some (name, ids) ->
     {p with Person.name = name; Person.identifiers = ids}

let person_cleanup p =
  let p = convert_gnd_person p in
  match Person.comma_split p with
  | Ok p -> p
  | Error p -> p
