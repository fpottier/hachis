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

(* This module implements hash sets. A hash set is represented in memory as
   a single flat array. Collisions are handled via linear probing. *)

(* For maximum performance, it is important to compile this code using
   flambda. The mainstream OCaml compiler does not inline functors and
   produces code that is about twice slower. *)

(* This module provides two functors, [Make_] and [Make]. The lower-level
   functor [Make_] expects an implementation of arrays of elements. The
   higher-level functor [Make] uses [Stdlib.Array], so it does not have
   this requirement. *)

(**The functor [Make_] takes three parameters: [H], [S], [K].

   The module [H] provides the type [H.t] of the set elements and equips this
   type with a hash function [hash] and an equivalence test [equal]. (Yes, the
   {i equivalence} function is conventionally named [equal].) The hash
   function must respect this equivalence: that is, [equiv x y] must imply
   [hash x = hash y].

   The module [S] provides two sentinel elements, which by convention are
   named [void] and [tomb]. These values must be distinct: [void != tomb] must
   hold. Furthermore, whenever the user inserts or looks up an element [x],
   this element must not be a sentinel: that is, [x != void && x != tomb] must
   hold.

   The module [K] is a minimal implementation of arrays. Only [make],
   [copy], [length], [unsafe_get], and [unsafe_set] are needed. *)
module Make_
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(_ : ARRAY with type element = H.t)
: SET with type element = H.t

(**The functor [Make] takes two parameters: [H] and [S].

   The module [H] provides the type [H.t] of the set elements and equips this
   type with a hash function [hash] and an equivalence test [equal]. (Yes, the
   {i equivalence} function is conventionally named [equal].) The hash
   function must respect this equivalence: that is, [equiv x y] must imply
   [hash x = hash y].

   The module [S] provides two sentinel elements, which by convention are
   named [void] and [tomb]. These values must be distinct: [void != tomb] must
   hold. Furthermore, whenever the user inserts or looks up an element [x],
   this element must not be a sentinel: that is, [x != void && x != tomb] must
   hold. *)
module Make
(H : HashedType)
(_ : SENTINELS with type t = H.t)
: SET with type element = H.t
