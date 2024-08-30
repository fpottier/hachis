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

(* This test builds a small hash set and prints it using [show].  *)

module V = struct
  type t = int
  let equal = Int.equal
  let hash = Hashtbl.hash
  let show = string_of_int
end

(* We use -1 and -2 as sentinels. *)
module S = struct type t = int let void = (-1) let tomb = (-2) end

(* Instantiate Hachis.HashSet. *)

module H =
  Hachis.HashSet.Make(V)(S)

(* Create and fill a hash set. *)

let n =
  20

let () =
  let s = H.create() in
  for x = 1 to n do
    H.add_absent s x
  done;
  print_string (H.show V.show s);
  flush stdout
