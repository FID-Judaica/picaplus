open Base
open Result.Monad_infix
open Abstract_fields
open Printf

type 'a string_tbl = (string,'a) Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

let result_to_option = Caml.Result.to_option

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



module Subfields = struct
  type t = (char * string) list
  let to_list t : (char * string) list = t

  let of_string sub_sep fld_str : t =
    let subs = Re.split sub_sep fld_str |> List.tl_exn in
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
  let to_title subs =
    let f main =
      let main = chop_parallel main in
      let sub = find_one ~tag:'d' subs >>| chop_parallel |> result_to_option in
      let name = find_one ~tag:'h' subs |> result_to_option in
      let script = get_script subs in
      Title.make ~main ~sub ~name ~script in
    find_one ~tag:'a' subs >>| f
  ;;
  let to_person_ppn subs = find_one ~tag:'9' subs
  let to_person subs =
    let get c = find_one ~tag:c subs in
    let identifiers = to_person_ppn subs |> Caml.Result.to_list in
    let partial = Person.make ~script:(get_script subs) ~identifiers in
    match get 'a', get 'd' with
    | Ok name, fn -> Some (partial ?first_name:(result_to_option fn) ~name)
    | Error _, Ok name -> Some (partial ?first_name:None ~name)
    | Error _, Error _ ->
       let rec loop = function
         | [] -> None
         | fld :: tl ->
            match get fld with
            | Ok name -> Some (partial ?first_name:None ~name)
            | _ -> loop tl in
       loop ['8'; 'P'; '9']
       
    
end

module Fields = struct
  type t = {
      data: string list;
      sub_sep: Re.re
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
end

module Record = struct
  let creator_codes = ["028A"; "028@"; "028P"]
  type t = {
      ppn: string;
      fields: string list string_tbl;
      sub_sep: Re.re
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
  let to_titles record =
    match find record ~label:"021A" with
    | Error _ -> []
    | Ok flds -> 
       Fields.subs flds
       |> List.map ~f:Subfields.to_title
       |> List.filter_map ~f:result_to_option
  ;;
  let to_creator_ppl record =
    let intellectual_creators =
      List.filter_map creator_codes
        ~f:(fun label -> find record ~label |> result_to_option) in
    let editors = find_numbered record ~label:"028B" in
    let others = match find record ~label:"208C" with
      | Error _ -> []
      | Ok flds -> [flds] in
    List.concat_map [intellectual_creators; others; editors]
      ~f:(fun flds ->
        List.concat_map ~f:Fields.subs flds
        |> List.filter_map ~f:Subfields.to_person)
    
  let to_years record =
    match find record ~label:"011@" with
    | Error _ -> []
    | Ok flds ->
       List.concat_map (Fields.subs flds) ~f:(fun subs ->
           let subs = Subfields.to_list subs in
           List.concat_map subs ~f:(fun (_, date_str) ->
               years_of_date_str date_str))
end
