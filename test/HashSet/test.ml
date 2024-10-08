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

open Monolith

(* We use nonnegative integer elements, and equip them with an equivalence
   relation that is not equality. We view two integer elements as equivalent
   if they are equal except possibly in their least significant bit. *)
module V = struct
  type t = int
  let normalize x = x lor 1
  let equal x y = Int.equal (normalize x) (normalize y)
  let hash x = Hashtbl.hash (normalize x)
end

(* We use -1 and -2 as sentinels. Our generators must be careful not
   to produce these elements. *)
module S = struct type t = int let void = (-1) let tomb = (-2) end

module R = Reference.Make(V)
module C = Hachis.HashSet.Make(V)(S)

(* -------------------------------------------------------------------------- *)

(* Print the above prologue when showing a failing test scenario. *)

let () = dprintf "          \
          module V = struct
            type t = int
            let normalize x = x lor 1
            let equal x y = Int.equal (normalize x) (normalize y)
            let hash x = Hashtbl.hash (normalize x)
          end
          module S = struct type t = int let void = (-1) let tomb = (-2) end
          open Hachis.HashSet.Make(V)(S)
"

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [set]. *)

let check _model =
  C.check, constant "check"

let set =
  declare_abstract_type ~check ()

(* We draw random non-sentinel (nonnegative) integer keys. *)

let element =
  semi_open_interval 0 32

(* We test [iter] by converting it to an [elements] function.
   This function must produce a sorted list, because the order
   in which elements are produced is unspecified. *)

let elements_of_iter iter s =
  let xs = ref [] in
  iter (fun x -> xs := x :: !xs) s;
  List.sort Int.compare !xs

let () = dprintf "          \
          let elements_of_iter iter s =
            let xs = ref [] in
            iter (fun x -> xs := x :: !xs) s;
            List.sort Int.compare !xs;;
"

(* We test [choose] by converting it to an emptiness test, as follows.
   Our wrapper ensures that the element returned by [choose] appears
   in the set. *)

let is_empty_of_choose mem choose s =
  try
    let x = choose s in
    assert (mem s x);
    false
  with Not_found ->
    true

let () = dprintf "          \
          let is_empty_of_choose mem choose s =
            try
              let x = choose s in
              assert (mem s x);
              false
            with Not_found ->
              true
"

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> set in
  declare "create" spec R.create C.create;

  let spec = set ^> element ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = set ^> element ^!> element in
  declare "find" spec R.find C.find;

  let spec = set ^> element ^> bool in
  declare "add_if_absent" spec R.add_if_absent C.add_if_absent;

  let spec = set ^> element ^> bool in
  declare "replace" spec R.replace C.replace;

  let spec = set ^> element ^!> element in
  declare "find_else_add" spec R.find_else_add C.find_else_add;

  let spec = set ^> element ^> unit in
  declare "remove" spec R.remove C.remove;

  let spec = set ^> element ^!> element in
  declare "find_and_remove" spec R.find_and_remove C.find_and_remove;

  let spec = set ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  let spec = set ^> unit in
  declare "tighten" spec R.tighten C.tighten;

  let spec = set ^> unit in
  declare "cleanup" spec R.cleanup C.cleanup;

  let spec = set ^> unit in
  declare "clear" spec R.clear C.clear;

  let spec = set ^> unit in
  declare "reset" spec R.reset C.reset;

  let spec = set ^> set in
  declare "copy" spec R.copy C.copy;

  let spec = set ^> list element in
  declare "elements_of_iter iter" spec
    (elements_of_iter R.iter) (elements_of_iter C.iter);

  let spec = set ^> bool in
  declare "is_empty_of_choose mem choose" spec
    R.is_empty (is_empty_of_choose C.mem C.choose);

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel
