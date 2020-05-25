open StdLabels

let seq_of_bytes ?encoding s =
  let decoder = Uutf.decoder ?encoding (`String s) in
  let rec seq () =
    match Uutf.decode decoder with
    | `End -> Seq.Nil
    | `Await -> failwith "shouldn't be able to get here"
    | `Malformed s -> Seq.Cons(Error s, seq)
    | `Uchar c -> Seq.Cons(Ok c, seq) in
  seq

let rev_list_of_bytes ?encoding s =
  let decoder = Uutf.decoder ?encoding (`String s) in
  let rec loop n acc =
    match Uutf.decode decoder with
    | `End -> Ok (n, acc)
    | `Await -> failwith "shouldn't be able to get here"
    | `Malformed s -> Error (s, Uutf.decoder_byte_count decoder)
    | `Uchar c -> loop (n+1) (c :: acc) in
  loop 0 []

let array_of_bytes ?encoding s =
  let f (len, list) =
    let a = Array.make len (Uchar.of_int 0) in
    let rec loop n = function
      | [] -> a
      | c :: tl -> Array.unsafe_set a n c;
                   loop (n-1) tl in
    loop (len-1) list in
  Result.map f (rev_list_of_bytes ?encoding s)

let array_to_bytes a ~buf ~encoding =
  Buffer.clear buf;
  let len = Array.length a in
  let encoder = Uutf.encoder encoding (`Buffer buf) in
  let rec loop n =
    if n >= len then
      match Uutf.encode encoder `End with
      | `Ok -> Ok (Buffer.contents buf)
      | `Partial -> Error "buffer full"
    else
      match `Uchar (Array.unsafe_get a n) |> Uutf.encode encoder with
      | `Ok -> loop (n+1)
      | `Partial -> Error "buffer full" in
  loop 0
