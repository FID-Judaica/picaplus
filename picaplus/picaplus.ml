open Base
open Result.Monad_infix

type 'a string_tbl = (string,'a) Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

let option_of_result = function Ok x -> Some x | Error _ -> None

module Subfields = struct
  type t = (char * string) list

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
  let get_lang (subs:t) =
    match find_one subs ~tag:'U' with
    | Error _ -> "Latn"
    | Ok script -> script
  ;;
  let chop_parallel str =
    match String.lsplit2 str ~on:'=' with
    | None -> str
    | Some (str, _) -> str
  ;;
  let to_title_of_021A subs =
    let f main =
      let main = chop_parallel main in
      let sub = find_one ~tag:'d' subs >>| chop_parallel |> option_of_result in
      let name = find_one ~tag:'h' subs |> option_of_result in
      let script = get_lang subs in
      Abstract_fields.Title.make ~main ~sub ~name ~script in
    find_one ~tag:'a' subs >>| f
  ;;
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
  let find_one_sub record ~label ~tag =
    find_one record ~label >>= Subfields.find_one ~tag
  ;;
  let to_titles record =
    match find record ~label:"021A" with
    | Error _ -> []
    | Ok flds -> 
       Fields.subs flds
       |> List.map ~f:Subfields.to_title_of_021A
       |> List.filter_map ~f:option_of_result
  ;;
end
