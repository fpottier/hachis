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

(**This module implements a hash set as a flat array, using linear probing. *)

(* For maximum performance, it is important to compile this code using
   flambda. The mainstream OCaml compiler does not inline functors and
   produces code that is about twice slower. *)

module type S = sig

  (**The type of the values contained in a set. *)
  type value

  (**The type of sets. A set always contains at most one value of each
     equivalence class: that is, [mem s x] and [mem s y] and [equiv x y]
     imply [x = y]. *)
  type set

  (**[create()] creates a fresh empty set. *)
  val create : unit -> set

  (**[mem s x] determines whether [x], or some equivalent value, is a member
     of the set [s]. *)
  val mem : set -> value -> bool

  (**[find s x] determines whether some value [y] that is equivalent to [x]
     is a member of the set [s]. If so, [y] is returned. Otherwise,
     [Not_found] is raised. *)
  val find : set -> value -> value

  (**If [x] or some equivalent value is a member of the set [s], then
     [add s x] has no effect and returns [false]. Otherwise, [add s x]
     inserts the value [x] into the set [s] and returns [true]. *)
  val add : set -> value -> bool

  (**[add_absent s x] inserts the value [x] into the set [s]. [x], or
     some equivalent value, must not already be a member of [s]. No
     result is returned. *)
  val add_absent : set -> value -> unit

  (**[find_else_add s x] determines whether some value [y] that is equivalent
     to [x] is a member of the set [s]. If so, [y] is returned. Otherwise, the
     value [x] is inserted into the set [s], and [Not_found] is raised.

     [find_else_add s x ] is equivalent to
     [try find s x with Not_found -> add_absent s x; raise Not_found]. *)
  val find_else_add : set -> value -> value

  (**If some value [y] that is equivalent to [x] is a member of the set
     [s], then [remove s x] removes [y] from the set [s] and returns
     [y]. Otherwise, this call has no effect and raises [Not_found]. *)
  val remove : set -> value -> value

  type population = int

  (**[population s] returns the number of inhabitants of the set [s]. *)
  val population : set -> population

  (**[cleanup s] cleans up the internal representation of the set by freeing
     the space occupied by any previously removed values. If many values
     have been recently removed from the set, this can free up some space
     and delay the need to grow the set's internal data array. *)
  val cleanup : set -> unit

  (**[clear s] empties the set [s]. The internal data array is retained,
     and is erased. This time complexity of this operation is linear in
     the size of the internal data array. *)
  val clear : set -> unit

  (**[reset s] empties the set [s]. The internal data array is abandoned.
     The time complexity of this operation is constant. *)
  val reset : set -> unit

  (**[copy s] returns a new set whose elements are the elements of [s].
     Its time complexity is linear in the size of the internal data array. *)
  val copy : set -> set

  (**[iter f s] applies the user-supplied function [f] in turn to each
     member of the set [s]. *)
  val iter : (value -> unit) -> set -> unit

  (**[show f s] returns a textual representation of the members of
     the set [s]. The user-supplied function [f] is used to obtain a
     textual representation of each member. *)
  val show : (value -> string) -> set -> string

  (**[statistics s] returns a string of information about the population,
     capacity and occupancy of the set [s]. *)
  val statistics : set -> string

  (**/**)
  (* In debug builds, [check s] checks that the set's internal invariant
     holds. In release builds, [check s] has no effect. *)
  val check : set -> unit

end

(**The functor [Make] takes three parameters: [A], [S], [V].

   The parameter [A] is a minimal implementation of arrays of values. Only
   [make], [length], [unsafe_get], and [unsafe_set] are needed.

   The parameter [S] provides two sentinel values, [void] and [tomb]. These
   values must be distinct: [void != tomb] must hold. Furthermore, whenever
   the user inserts or looks up a value [x] in the set, [x] must not be a
   sentinel: [x != void && x != tomb] must hold.

   The parameter [V] provides a hash function [hash] and an equivalence test
   [equal]. (Yes, the {i equivalence} function is conventionally named
   [equal].) The hash function must respect this equivalence: that is,
   [equiv x y] must imply [hash x = hash y]. *)
module Make
(A : sig
  type element
  type t
  val make : int -> element -> t
  val length : t -> int
  val unsafe_get : t -> int -> element
  val unsafe_set : t -> int -> element -> unit
end)
(S : sig
  type t = A.element
  val void : t
  val tomb : t
end)
(V : sig
  type t = S.t
  val hash  : t -> int
  val equal : t -> t -> bool
end)
: S with type value = V.t
