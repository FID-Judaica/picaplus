open Base
open Picaplus
open Stdio
module Seq = Caml.Seq
           

let get_encoder buff = Uutf.encoder `UTF_8 (`Buffer buff)

let get_first_codepoint str buff =
  let decoder = Uutf.decoder (`String str) in
  match Uutf.decode decoder with
  | `Uchar c ->
     Buffer.clear buff;
     let encode = Uutf.encode (get_encoder buff) in
     begin match encode (`Uchar c), encode (`End) with
       | `Ok, `Ok -> Ok (Buffer.contents buff)
       | _ -> Error "string was not encoded"
     end
  | `End -> Error "the string was empty"
  | _ -> Error "didn't get a char from the string"
;;

let get_first_codepoint_exn str buff =
  match get_first_codepoint str buff with
  | Ok chr -> chr
  | Error msg -> failwith msg
;;

let rec lines_of_ic ic = fun () ->
  match In_channel.input_line ic with
  | None -> Seq.Nil
  | Some line -> Seq.Cons(String.rstrip line, lines_of_ic ic)
;;

let get_ppn line =
  let rec get_it = function
    | "PPN:" :: tl -> List.hd_exn tl
    | _ :: tl -> get_it tl
    | [] -> failwith "No PPN found" in
  String.split line ~on:' '
  |> List.filter ~f:(fun s -> not String.(s = ""))
  |> get_it
;;

let rec get_record_lines lines =
  match lines () with
  | Seq.Nil -> [], None
  | Seq.Cons (line, tl) ->
     match line with
     | "" | "\r" -> [], Some tl
     | line ->
        let tail, lines = get_record_lines tl in
               line::tail, lines
;;

let get_record_lines lines =
  match lines () with
  | Seq.Nil -> [], None
  | Seq.Cons (_, tl) -> get_record_lines tl
;;

let rec line_records_of_lines lines =
  match lines () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (line, tl) ->
     match line with
     | "" -> line_records_of_lines tl
     | line ->
        if String.is_prefix line ~prefix:"SET:"
        then
          let ppn = get_ppn line in
          match get_record_lines tl with
          | [], _ -> failwith "record has no lines"
          | record_lines, None ->
             fun () ->
             Seq.Cons ((ppn, record_lines), Seq.empty)
          | record_lines, Some tl ->
             fun () ->
             Seq.Cons ((ppn, record_lines), line_records_of_lines tl)
        else
          failwith ("found line '" ^ line ^ "' while looking for ppn")
;;           

let rec record_of_line_record (ppn, lines) ~sub_sep ~buff =
  match !sub_sep with
  | None ->
     let (_, data) = List.hd_exn lines |> String.lsplit2_exn ~on:' ' in
     let chr = get_first_codepoint_exn data buff in
     sub_sep := Some (Re.Posix.compile_pat (Re.Pcre.quote chr));
     record_of_line_record (ppn, lines) ~sub_sep ~buff
  | Some sub_sep -> Record.of_lines ~ppn ~sub_sep lines
;;     

let records_of_lines lines =
  let buff = Buffer.create 4 in
  Seq.map
    (record_of_line_record ~buff ~sub_sep:(ref None))
    (line_records_of_lines lines)

let records_of_ic ic = records_of_lines (lines_of_ic ic)
let records_of_filename fn = records_of_ic (In_channel.create fn)

let _open_seq seq =
  match seq () with
  | Seq.Cons(hd, tl) -> hd, tl
  | _ -> failwith "oops"
