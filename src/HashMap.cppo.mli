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

(* This module implements hash sets. A hash maps is represented in memory
   using two flat arrays. Collisions are handled via linear probing. *)

(* For maximum performance, it is important to compile this code using
   flambda. The mainstream OCaml compiler does not inline functors and
   produces code that is about twice slower. *)

(* This module provides two functors, [Make_] and [Make]. The lower-level
   functor [Make_] expects an implementation of arrays of keys and an
   implementation of arrays of values. The higher-level functor [Make] uses
   [Stdlib.Array], so it does not have this requirement. *)

(**The functor [Make_] takes four parameters: [H], [S], [K], [V].

   The module [H] provides the type [H.t] of keys and equips this type with a
   hash function [hash] and an equivalence test [equal]. (Yes, the {i
   equivalence} function is conventionally named [equal].) The hash function
   must respect this equivalence: that is, [equiv x y] must imply
   [hash x = hash y].

   The module [S] provides two sentinel values, which by convention are named
   [void] and [tomb]. These values must be distinct: [void != tomb] must hold.
   Furthermore, whenever the user inserts or looks up a key [x], this key must
   not be a sentinel: that is, [x != void && x != tomb] must hold.

   The module [K] is a minimal implementation of arrays of keys. Only [make],
   [copy], [length], [unsafe_get], and [unsafe_set] are needed.

   The module [V] is a minimal implementation of arrays of values. Only
   [empty], [make], [copy], [length], [unsafe_get], and [unsafe_set] are
   needed. *)
module Make_
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(_ : ARRAY with type element = H.t)
(V : ARRAY)
: MAP with type key = H.t and type value = V.element

(**The functor [Make] takes three parameters: [H], [S], and [V].

   The module [H] provides the type [H.t] of keys and equips this type with a
   hash function [hash] and an equivalence test [equal]. (Yes, the {i
   equivalence} function is conventionally named [equal].) The hash function
   must respect this equivalence: that is, [equiv x y] must imply
   [hash x = hash y].

   The module [S] provides two sentinel keys, which by convention are named
   [void] and [tomb]. These values must be distinct: [void != tomb] must hold.
   Furthermore, whenever the user inserts or looks up a key [x], this key must
   not be a sentinel: that is, [x != void && x != tomb] must hold.

   The module [V] provides the type [V.t] of values. *)
module Make
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(V : sig (**The type of values. *) type t end)
: MAP with type key = H.t and type value = V.t
