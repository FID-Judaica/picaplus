type 'a string_tbl = (string, 'a) Base.Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

module Subfields :
  sig
    type t
    val of_string : Re.re -> string -> t
    val find : t -> tag:char -> string list
    val find_one : t -> tag:char -> (string, [> match_err ]) result
    val get_lang : t -> string
  end

module Fields :
  sig
    type t
    val make : string list -> Re.re -> t
    val subs : t -> Subfields.t list
    val find_sequence :
      t -> tag:char -> (string list * Subfields.t) Base.Sequence.t
    val find : t -> tag:char -> (string list * Subfields.t) list
    val find_one : t -> tag:char ->
      (string * Subfields.t, [> match_err]) result
  end

module Record :
  sig
    type t
    val make : string -> string list string_tbl -> Re.re -> t
    val of_lines : string list -> ppn:string -> sub_sep:Re.re -> t
    val fields : t -> string list string_tbl
    val find : t -> label:string -> (Fields.t, [> `NoMatch ]) result
    val find_one : t -> label:string ->
      (Subfields.t, [> match_err ]) result
    val find_one_sub : t -> label:string -> tag:char ->
      (string, [> match_err ]) result
  end

module Title :
  sig
    type t = {
      main : string;
      sub : string option;
      name : string option;
      script : string;
    }
    val of_021A_field : Subfields.t -> (t, [> match_err ]) result
    val s_of_picarecord : Record.t -> t list
    val repr : t -> string
  end
