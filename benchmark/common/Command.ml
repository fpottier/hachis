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

open Printf

(* -------------------------------------------------------------------------- *)

(* Prepare to read and parse the command line. *)

let specs : (Arg.key * Arg.spec * Arg.doc) list ref =
  ref []

let declare spec =
  specs := spec :: !specs

let parse () =
  let cmp (key1, _, _) (key2, _, _) = String.compare key1 key2 in
  let specs = List.sort cmp !specs in
  let anonymous _ = () in
  let usage = "Invalid usage" in
  Arg.parse (Arg.align specs) anonymous usage

(* -------------------------------------------------------------------------- *)

(* Reading various kinds of data from the command line. *)

let int (option : string) (default : int) (doc : string) : unit -> int =
  let v = ref default in
  declare (
    option,
    Arg.Set_int v,
    sprintf "<int> %s (default: %d)" doc !v
  );
  fun () -> !v

let string (option : string) (default : string) (doc : string) : unit -> string =
  let v = ref default in
  declare (
    option,
    Arg.Set_string v,
    sprintf "<string> %s (default: %s)" doc !v
  );
  fun () -> !v

let mandatory_string (option : string) (doc : string) : unit -> string =
  let v = ref None in
  declare (
    option,
    Arg.String (fun s -> v := Some s),
    sprintf "<string> %s" doc
  );
  fun () ->
    match !v with
    | None ->
        eprintf "Error: please specify %s <string>.\n%!" option; exit 1
    | Some s ->
        s

let optional_flag (option : string) (doc : string) : unit -> bool =
  let v = ref false in
  declare (
    option,
    Arg.Set v,
    sprintf " %s" doc
  );
  fun () -> !v
