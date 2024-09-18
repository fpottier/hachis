(******************************************************************************)
(*                                                                            *)
(*                                   Hachis                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

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
