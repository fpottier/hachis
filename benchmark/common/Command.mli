(* Declaring command line arguments. *)

(* [int option default doc] *)
val int : string -> int -> string -> (unit -> int)

(* [string option default doc] *)
val string : string -> string -> string -> (unit -> string)

(* [mandatory_string option doc] *)
val mandatory_string : string -> string -> (unit -> string)

(* [optional_flag option doc] *)
val optional_flag : string -> string -> (unit -> bool)

(* Parsing the command line. *)

val parse : unit -> unit
