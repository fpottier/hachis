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

(* A reference implementation of hash sets. *)

module Make (V : sig
  type t
  val hash  : t -> int
  val equal : t -> t -> bool
end) = struct

  module H = Hashtbl.Make(V)

  type set =
    V.t H.t

  let create () : set =
    H.create 32

  let mem s x =
    H.mem s x

  let find s x =
    H.find s x

  (* OCaml's hash tables implement multi-sets: an element can be
     present several times. We must compensate for this, and not
     add an element if it is already in the set. *)

  let add_if_absent s x =
    let was_present = H.mem s x in
    let was_absent = not was_present in
    if was_absent then
      H.add s x x;
    was_absent

  let replace s x =
    let was_present = H.mem s x in
    let was_absent = not was_present in
    H.replace s x x;
    was_absent

  let find_else_add s x =
    try
      find s x
    with Not_found ->
      ignore (add_if_absent s x);
      raise Not_found

  let remove =
    H.remove

  let find_and_remove s x =
    let y = H.find s x in
    H.remove s x;
    y

  let cardinal s =
    H.length s

  let is_empty s =
    cardinal s = 0

  let tighten _s =
    ()

  let cleanup _s =
    ()

  let clear =
    H.clear

  let reset =
    H.reset

  let copy =
    H.copy

  let iter f t =
    H.iter (fun x _ -> f x) t

end
