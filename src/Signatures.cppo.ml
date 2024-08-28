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

module type HashedType = sig

  (**A type of elements (in a hash set) or keys (in a hash map). *)
  type t

  (**An equivalence test on keys. The function [equal x y] returns
     [true] if and only if the keys [x] and [y] are equivalent. It is
     up to the user to define an equivalence relation on keys. In the
     simplest and most common case, equivalence is just equality. *)
  val equal : t -> t -> bool

  (**A hash function on keys. This function must be compatible with
     equivalence: that is, it must be the case that [equiv x y] implies
     [hash x = hash y]. *)
  val hash  : t -> int

end

module type SENTINELS = sig

  (**A type of elements (in a hash set) or keys (in a hash map). *)
  type t

  (**A sentinel value is a special value that must never be supplied as an
     argument to an operation such as [add] or [find]. A non-sentinel value
     [x] satisfies [x != void && x != tomb]. The sentinel values [void] and
     [tomb] must be distinct: that is, [void != tomb] must hold. *)
  val void : t

  (**A sentinel value is a special value that must never be supplied as an
     argument to an operation such as [add] or [find]. A non-sentinel value
     [x] satisfies [x != void && x != tomb]. The sentinel values [void] and
     [tomb] must be distinct: that is, [void != tomb] must hold. *)
  val tomb : t

end

module type ARRAY = sig

  (**The type of elements. *)
  type element

  (**The type of arrays. *)
  type t

  (**[make n x] returns a new array of length [n], where every slot contains
     the value [x]. *)
  val make : int -> element -> t

  (**[copy a] returns a new array whose length and content are those of
     the array [a]. *)
  val copy : t -> t

  (**[length a] returns the length of the array [a]. *)
  val length : t -> int

  (**[unsafe_get a i] returns the element found at index [i] in the array
     [a]. {b The index [i] must be valid}. *)
  val unsafe_get : t -> int -> element

  (**[unsafe_set a i x] writes the value [x] at index [i] in the array
     [a]. {b The index [i] must be valid}. *)
  val unsafe_set : t -> int -> element -> unit

end

module type SET = sig

  (**The type of the elements of a set. *)
  type element

  (**The type of sets. At all times, a set [s] contains at most one element of
     each equivalence class: that is, [mem s x] and [mem s y] and [equiv x y]
     imply [x = y]. *)
  type set

  (**[t] is a synonym for [set]. *)
  type t = set

  (** {2 Creation} *)

  (**[create()] creates a fresh empty set. *)
  val create : unit -> set

  (**[copy s] returns a new set whose elements are the elements of [s].
     Its time complexity is linear in the size of the internal data array. *)
  val copy : set -> set

  (** {2 Insertion} *)

  (**If [x] or some equivalent element is a member of the set [s], then
     [add s x] has no effect and returns [false]. Otherwise, [add s x]
     inserts the element [x] into the set [s] and returns [true]. *)
  val add : set -> element -> bool

  (**[add_absent s x] inserts the element [x] into the set [s]. No
     result is returned. {b The element [x], or an element [y] that is
     equivalent to [x], must not already be a member of [s]}: otherwise,
     the data structure would become inconsistent. It is recommended to
     guard this operation with [assert (not (mem s x))]. This allows the
     code to be both safe (when runtime assertions are enabled) and
     efficient (when runtime assertions are disabled). *)
  val add_absent : set -> element -> unit

  (** {2 Lookup} *)

  (**[mem s x] determines whether the element [x], or some element [y] that
     is equivalent to [x], is a member of the set [s]. *)
  val mem : set -> element -> bool

  (**[find s x] determines whether some element [y] that is equivalent to
     [x] is a member of the set [s]. If so, [y] is returned. Otherwise,
     [Not_found] is raised. *)
  val find : set -> element -> element

  (** {2 Insertion and lookup} *)

  (**[find_else_add s x] determines whether some element [y] that is equivalent
     to [x] is a member of the set [s]. If so, [y] is returned. Otherwise, the
     element [x] is inserted into the set [s], and [Not_found] is raised.

     [find_else_add s x] is equivalent to
     [try find s x with Not_found -> add_absent s x; raise Not_found]. *)
  val find_else_add : set -> element -> element

  (** {2 Deletion} *)

  (**If some element [y] that is equivalent to [x] is a member of the
     set [s], then [remove s x] removes [y] from the set [s] and returns
     [y]. Otherwise, the set [s] is unaffected, and [Not_found] is
     raised. *)
  val remove : set -> element -> element

  (** {2 Iteration} *)

  (**[foreach_key f s] applies the user-supplied function [f] in turn to
     each element [x] of the set [s]. {b The function [f] must not
     modify the set [s]}: that is, no elements can be inserted or
     deleted while iteration is ongoing. *)
  val foreach_key : (element -> unit) -> set -> unit

  (**[iter] is a synonym for [foreach_key]. *)
  val iter : (element -> unit) -> set -> unit

  (** {2 Cardinality} *)

  (**[population s] returns the cardinality of the set [s],
     that is, the number of inhabitants of this set. *)
  val population : set -> int

  (** {2 Management} *)

  (**[clear s] empties the set [s]. The internal data array is retained,
     and is erased. The time complexity of this operation is linear in
     the size of the internal data array. *)
  val clear : set -> unit

  (**[reset s] empties the set [s]. The internal data array is abandoned.
     The time complexity of this operation is constant. *)
  val reset : set -> unit

  (**[cleanup s] cleans up the internal representation of the set [s]
     by freeing up the space occupied by previously removed elements
     in the internal data array and by shrinking this data array, if
     possible. The time complexity of this operation is linear in the
     size of the internal data array. *)
  val cleanup : set -> unit

  (**[statistics s] returns a string of information about the population,
     capacity and occupancy of the set [s]. *)
  val statistics : set -> string

  (** {2 Display} *)

  (**[show show_key s] returns a textual representation of the set [s].
     This representation is delimited with curly braces. Two consecutive
     elements are separated with a comma and a space. The user-supplied
     function [show_key] is used to obtain a textual representation of
     each element. *)
  val show : (element -> string) -> set -> string

  (**/**)
  (* In debug builds, [check s] checks that the set's internal invariant
     holds. In release builds, [check s] has no effect. *)
  val check : set -> unit

end

module type MAP = sig

  (**The type of keys. *)
  type key

  (**The type of values. *)
  type value

  (**The type of maps. A map can be viewed as a set of pairs [(x, v)] of a
     key [x] and a value [v]. When a pair [(x, v)] exists in the map [m],
     we say that {i the key [x] is present with value [v]} in the map [m].
     At all times, a map [m] contains at most one key of each equivalence
     class: that is, [mem m x] and [mem m y] and [equiv x y] imply [x = y]. *)
  type map

  (**[t] is a synonym for [map]. *)
  type t = map

  (** {2 Creation} *)

  (**[create()] creates a fresh empty map. *)
  val create : unit -> map

  (**[copy m] returns a new map whose key-value bindings are those of [m].
     Its time complexity is linear in the size of the internal data arrays. *)
  val copy : map -> map

  (** {2 Insertion} *)

  (**If [x] or some equivalent element is present in the map [m], then
     [add m x v] has no effect and returns [false]. Otherwise, [add m x v]
     inserts the key [x] with value [v] into the map [m] and returns
     [true]. *)
  val add : map -> key -> value -> bool

  (**[add_absent m x v] inserts the key [x] with value [v] into the map
     [m]. No result is returned. {b The key [x], or a key [y] that is
     equivalent to [x], must not already be present in [m]}: otherwise,
     the data structure would become inconsistent. It is recommended to
     guard this operation with [assert (not (mem m x))]. This allows the
     code to be both safe (when runtime assertions are enabled) and
     efficient (when runtime assertions are disabled). *)
  val add_absent : map -> key -> value -> unit

  (** {2 Lookup} *)

  (**[mem m x] determines whether the key [x], or some key [y] that is
     equivalent to [x], is present in the map [m]. *)
  val mem : map -> key -> bool

  (**[find_key m x] determines whether some key [y] that is equivalent
     to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, [Not_found] is raised. *)
  val find_key : map -> key -> key

  (**[find_value m x] determines whether some key [y] that is equivalent
     to [x] is present with value [v] in the map [m]. If so, [v] is
     returned. Otherwise, [Not_found] is raised. *)
  val find_value : map -> key -> value

  (** {2 Insertion and lookup} *)

  (**[find_key_else_add m x] determines whether some key [y] that is
     equivalent to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, the key [x] with value [v] is inserted into the map [m],
     and [Not_found] is raised.

     [find_key_else_add m x v] is equivalent to
     [try find_key m x v with Not_found -> add_absent m x v; raise Not_found]. *)
  val find_key_else_add : map -> key -> value -> key

  (** {2 Deletion} *)

  (**If some key [y] that is equivalent to [x] is present in the map
     [m], then [remove m x] removes [y] from the map [m] and returns
     [y]. Otherwise, the map [m] is unaffected, and [Not_found] is
     raised. *)
  val remove : map -> key -> key

  (** {2 Iteration} *)

  (**[foreach_key f m] applies the user-supplied function [f] in turn to
     each key [x] in the map [m]. {b The function [f] must not modify
     the map [m]}: that is, no key-value pairs can be inserted or
     deleted while iteration is ongoing. *)
  val foreach_key : (key -> unit) -> map -> unit

  (**[foreach_key_value f m] applies the user-supplied function [f] in
     turn to each pair of a key [x] and value [v] in the map [m]. {b The
     function [f] must not modify the map [m]}: that is, no key-value
     pairs can be inserted or deleted while iteration is ongoing. *)
  val foreach_key_value : (key -> value -> unit) -> map -> unit

  (**[iter] is a synonym for [foreach_key_value]. *)
  val iter : (key -> value -> unit) -> map -> unit

  (** {2 Cardinality} *)

  (**[population m] returns the cardinality of the map [m],
     that is, the number of inhabitants of this map. *)
  val population : map -> int

  (** {2 Management} *)

  (**[clear m] empties the map [m]. The internal data arrays are retained,
     and are erased. The time complexity of this operation is linear in
     the size of the internal data arrays. *)
  val clear : map -> unit

  (**[reset m] empties the map [m]. The internal data arrays are abandoned.
     The time complexity of this operation is constant. *)
  val reset : map -> unit

  (**[cleanup m] cleans up the internal representation of the map [m]
     by freeing up the space occupied by previously removed elements
     in the internal data arrays and by shrinking these data arrays,
     if possible. The time complexity of this operation is linear in
     the size of the internal data arrays. *)
  val cleanup : map -> unit

  (**[statistics m] returns a string of information about the population,
     capacity and occupancy of the map [m]. *)
  val statistics : map -> string

  (** {2 Display} *)

  (**[show show_key show_value m] returns a textual representation of
     the map [m]. The user-supplied functions [show_key] and
     [show_value] are used to obtain textual representations of keys
     and values. *)
  val show : (key -> string) -> (value -> string) -> map -> string

  (**/**)
  (* In debug builds, [check m] checks that the map's internal invariant
     holds. In release builds, [check m] has no effect. *)
  val check : map -> unit

end
