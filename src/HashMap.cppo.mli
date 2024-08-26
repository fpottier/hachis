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

module Make_
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(_ : ARRAY with type element = H.t)
(V : sig include ARRAY val empty : t end)
: MAP with type key = H.t and type value = V.element

module Make
(H : HashedType)
(_ : SENTINELS with type t = H.t)
(V : sig type t end)
: MAP with type key = H.t and type value = V.t
