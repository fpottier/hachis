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

  (**[empty] is the empty array. *)
  val empty : t

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

  (**[fill a o k x] fills the array segment identified by array [a],
     offset [o], and length [k] with the value [x]. *)
  val fill : t -> int -> int -> element -> unit

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

  (**[create()] creates a fresh empty set.

     Time complexity: {m O(1)}. *)
  val create : unit -> set

  (**[copy s] returns a new set whose elements are the elements of [s].

     Time complexity: {m O(c)},
     where {m c} is the capacity of the set [s]. *)
  val copy : set -> set

  (** {2 Insertion} *)

  (**If [x] or some equivalent element is a member of the set [s], then
     [add_if_absent s x] has no effect and returns [false]. Otherwise,
     [add_if_absent s x] inserts the element [x] into the set [s] and
     returns [true].

     Thus, [add_if_absent s x] returns [true] if and only if the cardinality
     of the set [s] increases as a result of this operation.

     If necessary, the capacity of the set [s] is increased.

     Time complexity: {m O(1)}. *)
  val add_if_absent : set -> element -> bool

  (**[add_absent s x] inserts the element [x] into the set [s]. No
     result is returned. {b The element [x], or an element [y] that is
     equivalent to [x], must not already be a member of [s]}: otherwise,
     the data structure would become inconsistent. It is recommended to
     guard this operation with [assert (not (mem s x))]. This allows the
     code to be both safe (when runtime assertions are enabled) and
     efficient (when runtime assertions are disabled).

     If necessary, the capacity of the set [s] is increased.

     Time complexity: {m O(1)}. *)
  val add_absent : set -> element -> unit

  (**If some element that is equivalent to [x] is present in the set [s],
     then [replace s x] removes this pre-existing element, inserts [x]
     into the set [s], and returns [false]. Otherwise, [replace s x]
     inserts [x] into the set [s] and returns [true].

     Thus, [replace s x] returns [true] if and only if the cardinality of
     the set [s] increases as a result of this operation.

     If necessary, the capacity of the set [s] is increased.

     Time complexity: {m O(1)}. *)
  val replace : set -> element -> bool

  (** {2 Lookup} *)

  (**[mem s x] determines whether the element [x], or some element [y] that
     is equivalent to [x], is a member of the set [s].

     Time complexity: {m O(1)}. *)
  val mem : set -> element -> bool

  (**[find s x] determines whether some element [y] that is equivalent to
     [x] is a member of the set [s]. If so, [y] is returned. Otherwise,
     [Not_found] is raised.

     Time complexity: {m O(1)}. *)
  val find : set -> element -> element

  (**If the set [s] has nonzero cardinality, then [choose s] returns an
     element of the set [s]. This element is chosen at random. Otherwise,
     [choose s] raises [Not_found].

     Time complexity: {m O(c)} in the worst case
     and {m O(c/n)} in expectation,
     where {m c} is the capacity of the set [s]
     and {m n} is its cardinality.

     If the occupancy rate {m n/c} remains above a certain fixed
     threshold, then these bounds can be written under the form
     {m O(n)} in the worst case and {m O(1)} in expectation.

     If [choose] is used in a loop where elements are removed from the
     set then it is recommended to monitor the set's occupancy rate.
     If the occupancy rate drops below {m 1/4}, call [cleanup] so as
     to shrink the table and guarantee a higher occupancy rate. *)
  val choose : set -> element

  (** {2 Insertion and lookup} *)

  (**[find_else_add s x] determines whether some element [y] that is equivalent
     to [x] is a member of the set [s]. If so, [y] is returned. Otherwise, the
     element [x] is inserted into the set [s], and [Not_found] is raised.

     [find_else_add s x] is equivalent to
     [try find s x with Not_found -> add_absent s x; raise Not_found].

     Time complexity: {m O(1)}. *)
  val find_else_add : set -> element -> element

  (** {2 Deletion} *)

  (**If some element [y] that is equivalent to [x] is a member of the
     set [s], then [remove s x] removes [y] from the set [s].
     Otherwise, nothing happens.

     Time complexity: {m O(1)}. *)
  val remove : set -> element -> unit

  (**If some element [y] that is equivalent to [x] is a member of the set
     [s], then [find_and_remove s x] removes [y] from the set [s] and
     returns [y]. Otherwise, the set [s] is unaffected, and [Not_found] is
     raised.

     Time complexity: {m O(1)}. *)
  val find_and_remove : set -> element -> element

  (** {2 Iteration} *)

  (**[foreach_key f s] applies the user-supplied function [f] in turn to
     each element [x] of the set [s]. {b The function [f] must not
     modify the set [s]}: that is, no elements can be inserted or
     deleted while iteration is ongoing.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the set [s]. *)
  val foreach_key : (element -> unit) -> set -> unit

  (**[iter] is a synonym for [foreach_key]. *)
  val iter : (element -> unit) -> set -> unit

  (** {2 Cardinality} *)

  (**[cardinal s] returns the cardinality of the set [s],
     that is, the number of inhabitants of this set.

     Time complexity: {m O(1)}. *)
  val cardinal : set -> int

  (**[is_empty s] is equivalent to [cardinal s = 0].

     Time complexity: {m O(1)}. *)
  val is_empty : set -> bool

  (** {2 Cleanup} *)

  (**[clear s] empties the set [s]. The internal data array is retained,
     and is erased. Thus, the capacity of the set [s] is unchanged.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the set [s]. *)
  val clear : set -> unit

  (**[reset s] empties the set [s]. The internal data array is abandoned.
     Thus, the capacity of the set [s] is reset to a small constant.

     Time complexity: {m O(1)}. *)
  val reset : set -> unit

  (**[cleanup s] cleans up the internal representation of the set [s], as
     follows. First, it eliminates the tombstones that earlier deletion
     operations may have created in the internal data array. Second, it
     decreases the capacity of the set [s], if necessary, so as to ensure that
     the occupancy rate {m n/c} is high enough. When [cleanup] returns, the
     occupancy rate is guaranteed to be at least 1/4; so {m c} is {m O(n)}.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the set [s]. *)
  val cleanup : set -> unit

  (** {2 Display} *)

  (**[show show_key s] returns a textual representation of the set [s].
     This representation is delimited with curly braces. Two consecutive
     elements are separated with a comma and a space. The user-supplied
     function [show_key] is used to obtain a textual representation of
     each element.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the set [s]. *)
  val show : (element -> string) -> set -> string

  (** {2 Statistics} *)

  (**[capacity s] returns the current capacity of the set [s], that is,
     the current size of its internal array.

     Time complexity: {m O(1)}. *)
  val capacity : set -> int

  (**[occupation s] returns the current occupation of the set [s],
     that is, the number of occupied entries in its internal data
     array. This number may be greater than [cardinal s].

     Time complexity: {m O(1)}. *)
  val occupation : set -> int

  (**We say that a key [x] requires a search of length [k] in the set [s]
     if the function call [mem s x] requires reading [k+1] successive
     slots in the internal data array of the set [s]. In the best case, an
     element requires a search of length 0. If there are collisions, then
     some elements will require a search of length more than 0.

     A search length histogram for the set [s] is a finite association map
     that maps a natural integer [k] to the number of elements that
     require a search of length [k] in the set [s]. *)
  type histogram = int Map.Make(Int).t

  (**[histogram s] returns a histogram of the search lengths for the
     set [s].

     Time complexity: {m O(c \log c)},
     where {m c} is the capacity of the set [s]. *)
  val histogram : set -> histogram

  (**[average h] returns the average search length in the histogram [h].

     Time complexity: {m O(n)},
     where {m n} is the cardinality of the set [s]. *)
  val average : histogram -> float

  (**[statistics s] returns a string of information that includes the
     cardinality, capacity, occupancy rate, average search length,
     and search length histogram of the set [s].

     Time complexity: {m O(c \log c)},
     where {m c} is the capacity of the set [s]. *)
  val statistics : set -> string

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

  (**[create()] creates a fresh empty map.

     Time complexity: {m O(1)}. *)
  val create : unit -> map

  (**[copy m] returns a new map whose key-value bindings are those of [m].

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val copy : map -> map

  (** {2 Insertion} *)

  (**If [x] or some equivalent key is present in the map [m], then
     [add_if_absent m x v] has no effect and returns [false]. Otherwise,
     [add_if_absent m x v] inserts the key [x] with value [v] into the map
     [m] and returns [true].

     Thus, [add_if_absent m x v] returns [true] if and only if the
     cardinality of the map [m] increases as a result of this operation.

     If necessary, the capacity of the map [m] is increased.

     Time complexity: {m O(1)}. *)
  val add_if_absent : map -> key -> value -> bool

  (**[add_absent m x v] inserts the key [x] with value [v] into the map
     [m]. No result is returned. {b The key [x], or a key [y] that is
     equivalent to [x], must not already be present in [m]}: otherwise,
     the data structure would become inconsistent. It is recommended to
     guard this operation with [assert (not (mem m x))]. This allows the
     code to be both safe (when runtime assertions are enabled) and
     efficient (when runtime assertions are disabled).

     If necessary, the capacity of the map [m] is increased.

     Time complexity: {m O(1)}. *)
  val add_absent : map -> key -> value -> unit

  (**If some key that is equivalent to [x] is present in the map [m], then
     [replace m x v] removes this pre-existing key and its value, inserts
     the key [x] with value [v] into the map [m], and returns [false].
     Otherwise, [replace m x v] inserts the key [x] with value [v] into
     the map [m] and returns [true].

     Thus, [replace m x v] returns [true] if and only if the cardinality
     of the map [m] increases as a result of this operation.

     If necessary, the capacity of the map [m] is increased.

     Time complexity: {m O(1)}. *)
  val replace : map -> key -> value -> bool

  (** {2 Lookup} *)

  (**[mem m x] determines whether the key [x], or some key [y] that is
     equivalent to [x], is present in the map [m].

     Time complexity: {m O(1)}. *)
  val mem : map -> key -> bool

  (**[find_key m x] determines whether some key [y] that is equivalent
     to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, [Not_found] is raised.

     Time complexity: {m O(1)}. *)
  val find_key : map -> key -> key

  (**[find_value m x] determines whether some key [y] that is equivalent
     to [x] is present with value [v] in the map [m]. If so, [v] is
     returned. Otherwise, [Not_found] is raised.

     Time complexity: {m O(1)}. *)
  val find_value : map -> key -> value

  (**[find] is a synonym for [find_value]. *)
  val find : map -> key -> value

  (**If the map [m] has nonzero cardinality, then [choose m] returns
     a key that is present in the map [m]. This key is chosen at random.
     Otherwise, [choose m] raises [Not_found].

     Time complexity: {m O(c)} in the worst case
     and {m O(c/n)} in expectation,
     where {m c} is the capacity of the map [m]
     and {m n} is its cardinality.

     If the occupancy rate {m n/c} remains above a certain fixed
     threshold, then these bounds can be written under the form
     {m O(n)} in the worst case and {m O(1)} in expectation.

     If [choose] is used in a loop where entries are removed from the
     map then it is recommended to monitor the map's occupancy rate.
     If the occupancy rate drops below {m 1/4}, call [cleanup] so as
     to shrink the table and guarantee a higher occupancy rate. *)
  val choose : map -> key

  (** {2 Insertion and lookup} *)

  (**[find_key_else_add m x] determines whether some key [y] that is
     equivalent to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, the key [x] with value [v] is inserted into the map [m],
     and [Not_found] is raised.

     [find_key_else_add m x v] is equivalent to
     [try find_key m x v with Not_found -> add_absent m x v; raise Not_found].

     Time complexity: {m O(1)}. *)
  val find_key_else_add : map -> key -> value -> key

  (**[find_value_else_add m x] determines whether some key [y] that is
     equivalent to [x] is present in the map [m] with value [v]. If
     so, [v] is returned. Otherwise, the key [x] with value [v] is
     inserted into the map [m], and [Not_found] is raised.

     [find_value_else_add m x v] is equivalent to
     [try find_value m x v with Not_found -> add_absent m x v; raise Not_found].

     Time complexity: {m O(1)}. *)
  val find_value_else_add : map -> key -> value -> value

  (** {2 Deletion} *)

  (**If some key [y] that is equivalent to [x] is present in the map [m],
     then [remove m x] removes [y] from the map [m]. Otherwise, nothing
     happens.

     Time complexity: {m O(1)}. *)
  val remove : map -> key -> unit

  (**If some key [y] that is equivalent to [x] is present in the map [m],
     then [find_key_and_remove m x] removes [y] from the map [m] and returns
     [y]. Otherwise, the map [m] is unaffected, and [Not_found] is raised.

     Time complexity: {m O(1)}. *)
  val find_key_and_remove : map -> key -> key

  (**If some key [y] that is equivalent to [x] is present with value [v] in
     the map [m], then [find_value_and_remove m x] removes [y] from the map
     [m] and returns [v]. Otherwise, the map [m] is unaffected, and
     [Not_found] is raised.

     Time complexity: {m O(1)}. *)
  val find_value_and_remove : map -> key -> value

  (** {2 Iteration} *)

  (**[foreach_key f m] applies the user-supplied function [f] in turn to
     each key [x] in the map [m]. {b The function [f] must not modify
     the map [m]}: that is, no key-value pairs can be inserted or
     deleted while iteration is ongoing.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val foreach_key : (key -> unit) -> map -> unit

  (**[foreach_key_value f m] applies the user-supplied function [f] in
     turn to each pair of a key [x] and value [v] in the map [m]. {b The
     function [f] must not modify the map [m]}: that is, no key-value
     pairs can be inserted or deleted while iteration is ongoing.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val foreach_key_value : (key -> value -> unit) -> map -> unit

  (**[iter] is a synonym for [foreach_key_value]. *)
  val iter : (key -> value -> unit) -> map -> unit

  (** {2 Cardinality} *)

  (**[cardinal m] returns the cardinality of the map [m],
     that is, the number of inhabitants of this map.

     Time complexity: {m O(1)}. *)
  val cardinal : map -> int

  (**[is_empty m] is equivalent to [cardinal m = 0].

     Time complexity: {m O(1)}. *)
  val is_empty : map -> bool

  (** {2 Cleanup} *)

  (**[clear m] empties the map [m]. The internal data arrays are retained,
     and are erased. Thus, the capacity of the map [m] is unchanged.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val clear : map -> unit

  (**[reset m] empties the map [m]. The internal data arrays are abandoned.
     Thus, the capacity of the map [m] is reset to a small constant.

     Time complexity: {m O(1)}. *)
  val reset : map -> unit

  (**[cleanup m] cleans up the internal representation of the map [m], as
     follows. First, it eliminates the tombstones that earlier deletion
     operations may have created in the internal data arrays. Second, it
     decreases the capacity of the map [m], if necessary, so as to ensure that
     the occupancy rate {m n/c} is high enough. When [cleanup] returns, the
     occupancy rate is guaranteed to be at least 1/4; so {m c} is {m O(n)}.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val cleanup : map -> unit

  (** {2 Display} *)

  (**[show show_key show_value m] returns a textual representation of
     the map [m]. The user-supplied functions [show_key] and
     [show_value] are used to obtain textual representations of keys
     and values.

     Time complexity: {m O(c)},
     where {m c} is the capacity of the map [m]. *)
  val show : (key -> string) -> (value -> string) -> map -> string

  (** {2 Statistics} *)

  (**[capacity m] returns the current capacity of the map [m], that is,
     the current size of its internal data arrays.

     Time complexity: {m O(1)}. *)
  val capacity : map -> int

  (**[occupation m] returns the current occupation of the map [m],
     that is, the number of occupied entries in its internal data
     arrays. This number may be greater than [cardinal m].

     Time complexity: {m O(1)}. *)
  val occupation : map -> int

  (**We say that a key [x] requires a search of length [k] in the map [m]
     if the function call [mem m x] requires reading [k+1] successive
     slots in the internal data array of the map [m]. In the best case,
     a key requires a search of length 0. If there are collisions, then
     some keys will require a search of length more than 0.

     A search length histogram for the map [m] is a finite association map
     that maps a natural integer [k] to the number of keys that require a
     search of length [k] in the map [m]. *)
  type histogram = int Map.Make(Int).t

  (**[histogram m] returns a histogram of the search lengths for the map
     [m].

     Time complexity: {m O(c\log c)},
     where {m c} is the capacity of the map [m]. *)
  val histogram : map -> histogram

  (**[average h] returns the average search length in the histogram [h].

     Time complexity: {m O(n)},
     where {m n} is the cardinality of the map [m]. *)
  val average : histogram -> float

  (**[statistics m] returns a string of information that includes the
     cardinality, capacity, occupancy rate, average search length,
     and search length histogram of the map [m].

     Time complexity: {m O(c \log c)},
     where {m c} is the capacity of the map [m]. *)
  val statistics : map -> string

  (**/**)
  (* In debug builds, [check m] checks that the map's internal invariant
     holds. In release builds, [check m] has no effect. *)
  val check : map -> unit

end
