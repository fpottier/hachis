(******************************************************************************)
(*                                                                            *)
(*                                   Hachis                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(******************************************************************************)

(* A reference implementation of hash maps. *)

module Make (K : sig
  type t
  val hash  : t -> int
  val equal : t -> t -> bool
end)
(V : sig type t end)
= struct

  module H = Hashtbl.Make(K)

  type map =
    V.t H.t

  let create () : map =
    H.create 32

  let mem m x =
    H.mem m x

  let find_key m x =
    (* [find_key] is not directly supported by OCaml's hash tables.
       We use an inefficient method based on iteration. *)
    let exception Found of K.t in
    try
      H.iter (fun y _v ->
        if K.equal x y then raise (Found y)
      ) m;
      raise Not_found
    with Found y ->
      y

  let find_value m x =
    H.find m x

  (* OCaml's hash tables implement multi-maps: an element can be
     present several times. We must compensate for this, and not
     add an element if it is already in the map. *)

  let add m x v =
    let was_present = H.mem m x in
    let was_absent = not was_present in
    if was_absent then
      H.add m x v;
    was_absent

  let add_absent m x v =
    let was_present = H.mem m x in
    assert (not was_present);
    H.add m x v

  let find_key_else_add m x v =
    try
      find_key m x
    with Not_found ->
      add_absent m x v;
      raise Not_found

  let remove m x =
    let y = find_key m x in
    H.remove m x;
    y

  let population m =
    H.length m

  let cleanup _m =
    ()

  let clear =
    H.clear

  let reset =
    H.reset

  let copy =
    H.copy

  let iter =
    H.iter

end
