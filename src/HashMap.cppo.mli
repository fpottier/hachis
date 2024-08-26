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

#include "Signatures.cppo.ml"

module Make
(K : ARRAY)
(_ : SENTINELS with type t = K.element)
(_ : HashedType with type t = K.element)
(V : sig include ARRAY val empty : t end)
: MAP with type key = K.element and type value = V.element
