type 'a string_tbl = (string, 'a) Base.Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

module Subfields :
  sig
    type t
    val to_list : t -> (char * string) list
    val of_string : Re.re -> string -> t
    val find : t -> tag:char -> string list
    val find_one : t -> tag:char -> (string, [> match_err ]) result
    val get_script : t -> string
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
    val to_titles : t -> Abstract_fields.Title.t list
    val to_creator_ppl :
      ?sub_func:(Subfields.t -> Abstract_fields.Person.t option)
      -> t -> Abstract_fields.Person.t list
    val to_years : t -> int list
    val get_ppn : t -> string
  end
