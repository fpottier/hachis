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

module R = Reference.Make(V)(V)
module C = Hachis.HashMap.Make(V)(S)(V)

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
          open Hachis.HashMap.Make(V)(S)(V)
"

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [map]. *)

let check _model =
  C.check, constant "check"

let map =
  declare_abstract_type ~check ()

(* We draw random non-sentinel (nonnegative) integer keys. *)

let key =
  semi_open_interval 0 32

(* We draw random integer values. *)

let value =
  semi_open_interval 0 32

(* The absence of a key in a set
   is a precondition of [add_absent]. *)

let absent (s : R.map) (x : int) : bool =
  not (R.mem s x)

(* We test [iter] by converting it to a [bindings] function.
   This function must produce a sorted list, because the order
   in which bindings are produced is unspecified. *)

let bindings_of_iter iter m =
  let xvs = ref [] in
  iter (fun x v -> xvs := (x, v) :: !xvs) m;
  let compare (x1, _) (x2, _) = Int.compare x1 x2 in
  List.sort compare !xvs

let () = dprintf "          \
          let bindings_of_iter iter m =
            let xvs = ref [] in
            iter (fun x v -> xvs := (x, v) :: !xvs) m;
            let compare (x1, _) (x2, _) = Int.compare x1 x2 in
            List.sort compare !xvs;;
"

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> map in
  declare "create" spec R.create C.create;

  let spec = map ^> key ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = map ^> key ^!> key in
  declare "find_key" spec R.find_key C.find_key;

  let spec = map ^> key ^!> value in
  declare "find_value" spec R.find_value C.find_value;

  let spec = map ^> key ^> value ^> bool in
  declare "add_if_absent" spec R.add_if_absent C.add_if_absent;

  let spec = map ^>> fun s -> (absent s) % key ^> value ^> unit in
  declare "add_absent" spec R.add_absent C.add_absent;

  let spec = map ^> key ^> value ^> bool in
  declare "replace" spec R.replace C.replace;

  let spec = map ^> key ^> value ^!> key in
  declare "find_key_else_add" spec
    R.find_key_else_add C.find_key_else_add;

  let spec = map ^> key ^> value ^!> value in
  declare "find_value_else_add" spec
    R.find_value_else_add C.find_value_else_add;

  let spec = map ^> key ^> unit in
  declare "remove" spec R.remove C.remove;

  let spec = map ^> key ^!> key in
  declare "find_key_and_remove" spec
    R.find_key_and_remove C.find_key_and_remove;

  let spec = map ^> key ^!> value in
  declare "find_value_and_remove" spec
    R.find_value_and_remove C.find_value_and_remove;

  let spec = map ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  let spec = map ^> unit in
  declare "cleanup" spec R.cleanup C.cleanup;

  let spec = map ^> unit in
  declare "clear" spec R.clear C.clear;

  let spec = map ^> unit in
  declare "reset" spec R.reset C.reset;

  let spec = map ^> map in
  declare "copy" spec R.copy C.copy;

  let spec = map ^> list (key *** value) in
  declare "bindings_of_iter iter" spec
    (bindings_of_iter R.iter) (bindings_of_iter C.iter);

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel
