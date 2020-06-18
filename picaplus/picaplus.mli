type 'a string_tbl = (string, 'a) Base.Hashtbl.t
type match_err = [`NoMatch | `MultiMatch]

module Subfields :
  sig
    type t
    val to_list : t -> (char * string) list
    val of_list : (char * string) list -> t
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
    val to_titles : ?label:string -> t -> Abstract_fields.Title.t list
    val to_series : t -> Abstract_fields.Title.t list
    val to_creator_ppl :
      ?sub_func:(Subfields.t -> Abstract_fields.Person.t option)
      -> t -> Abstract_fields.Person.t list
    val to_years : t -> int list
    val to_publisher : t -> Abstract_fields.Publisher.t list
    val get_ppn : t -> string
    val to_api_json :
      ?person_cleanup:(Abstract_fields.Person.t -> Abstract_fields.Person.t)
      -> t -> Yojson.t
  end

val get_gnd_name : string -> (string * string list) option
val convert_gnd_person : Abstract_fields.Person.t -> Abstract_fields.Person.t
val person_cleanup : Abstract_fields.Person.t -> Abstract_fields.Person.t
