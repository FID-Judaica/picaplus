let update_buff_from_string ?encoding ~buf s =
  let decoder = Uutf.decoder ?encoding s in
  let rec loop n = function
    | `End -> Ok (n+1)
    | `Await -> failwith "shouldn't be able to get here"
    | `Malformed s -> Error s
    | `Uchar c ->
       buf.(n) <- Uchar.to_int c;
       loop (n+1) (Uutf.decode decoder) in
  loop 0 (Uutf.decode decoder)
