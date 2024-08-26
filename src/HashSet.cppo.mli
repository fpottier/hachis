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

#include "Signatures.cppo.ml"

(**This module implements a hash set as a flat array, using linear probing. *)

(* For maximum performance, it is important to compile this code using
   flambda. The mainstream OCaml compiler does not inline functors and
   produces code that is about twice slower. *)

(**The functor [Make_] takes three parameters: [H], [S], [A].

   The module [H] provides a hash function [hash] and an equivalence test
   [equal]. (Yes, the {i equivalence} function is conventionally named
   [equal].) The hash function must respect this equivalence: that is,
   [equiv x y] must imply [hash x = hash y].

   The module [S] provides two sentinel values, [void] and [tomb]. These
   values must be distinct: [void != tomb] must hold. Furthermore, whenever
   the user inserts or looks up an element [x], this element must not be a
   sentinel: that is, [x != void && x != tomb] must hold.

   The module [A] is a minimal implementation of arrays. Only [make],
   [copy], [length], [unsafe_get], and [unsafe_set] are needed. *)
module Make_
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(_ : ARRAY with type element = H.t)
: SET with type element = H.t

module Make
(H : HashedType)
(_ : SENTINELS with type t = H.t)
: SET with type element = H.t
