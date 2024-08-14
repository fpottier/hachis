module type ARRAY = sig
  type element
  type t
  val make : int -> element -> t
  val copy : t -> t
  val length : t -> int
  val unsafe_get : t -> int -> element
  val unsafe_set : t -> int -> element -> unit
end

module type SENTINELS = sig
  type t
  val void : t
  val tomb : t
end

module type HashedType = sig
  type t
  val hash  : t -> int
  val equal : t -> t -> bool
end

module type SET = sig

  (**The type of the elements of a set. *)
  type element

  (**The type of sets. A set [s] always contains at most one element
     in each equivalence class: that is, [mem s x] and [mem s y] and
     [equiv x y] imply [x = y]. *)
  type set

  (**[create()] creates a fresh empty set. *)
  val create : unit -> set

  (**[mem s x] determines whether [x], or some equivalent element, is a
     member of the set [s]. *)
  val mem : set -> element -> bool

  (**[find s x] determines whether some element [y] that is equivalent to
     [x] is a member of the set [s]. If so, [y] is returned. Otherwise,
     [Not_found] is raised. *)
  val find : set -> element -> element

  (**If [x] or some equivalent element is a member of the set [s], then
     [add s x] has no effect and returns [false]. Otherwise, [add s x]
     inserts the element [x] into the set [s] and returns [true]. *)
  val add : set -> element -> bool

  (**[add_absent s x] inserts the element [x] into the set [s]. [x], or
     some equivalent element, must not already be a member of [s]. No
     result is returned. *)
  val add_absent : set -> element -> unit

  (**[find_else_add s x] determines whether some element [y] that is equivalent
     to [x] is a member of the set [s]. If so, [y] is returned. Otherwise, the
     element [x] is inserted into the set [s], and [Not_found] is raised.

     [find_else_add s x] is equivalent to
     [try find s x with Not_found -> add_absent s x; raise Not_found]. *)
  val find_else_add : set -> element -> element

  (**If some element [y] that is equivalent to [x] is a member of the set
     [s], then [remove s x] removes [y] from the set [s] and returns
     [y]. Otherwise, this call has no effect and raises [Not_found]. *)
  val remove : set -> element -> element

  type population = int

  (**[population s] returns the number of inhabitants of the set [s]. *)
  val population : set -> population

  (**[cleanup s] cleans up the internal representation of the set [s] by
     freeing the space occupied by any previously removed elements. If
     many elements have been recently removed from the set, this can free
     up some space and delay the need to grow the set's internal data
     array. *)
  val cleanup : set -> unit

  (**[clear s] empties the set [s]. The internal data array is retained,
     and is erased. The time complexity of this operation is linear in
     the size of the internal data array. *)
  val clear : set -> unit

  (**[reset s] empties the set [s]. The internal data array is abandoned.
     The time complexity of this operation is constant. *)
  val reset : set -> unit

  (**[copy s] returns a new set whose elements are the elements of [s].
     Its time complexity is linear in the size of the internal data array. *)
  val copy : set -> set

  (**[foreach_key f s] applies the user-supplied function [f] in turn to
     each element [x] of the set [s]. *)
  val foreach_key : (element -> unit) -> set -> unit

  (**[iter] is a synonym for [foreach_key]. *)
  val iter : (element -> unit) -> set -> unit

  (**[show f s] returns a textual representation of the set [s]. The
     user-supplied function [f] is used to obtain a textual representation
     of each element. *)
  val show : (element -> string) -> set -> string

  (**[statistics s] returns a string of information about the population,
     capacity and occupancy of the set [s]. *)
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

  (**The type of maps. A map [m] always contains at most one key in
     each equivalence class: that is, [mem m x] and [mem m y] and
     [equiv x y] imply [x = y]. *)
  type map

  (**[create()] creates a fresh empty map. *)
  val create : unit -> map

  (**[mem m x] determines whether the key [x], or some equivalent key,
     is present in the map [m]. *)
  val mem : map -> key -> bool

  (**[find_key m x] determines whether some key [y] that is equivalent
     to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, [Not_found] is raised. *)
  val find_key : map -> key -> key

  (**[find_value m x] determines whether some key [y] that is equivalent
     to [x] is present with value [v] in the map [m]. If so, [v] is
     returned. Otherwise, [Not_found] is raised. *)
  val find_value : map -> key -> value

  (**If [x] or some equivalent element is present in the map [m], then
     [add m x v] has no effect and returns [false]. Otherwise, [add m x v]
     inserts the key [x] with value [v] into the map [m] and returns
     [true]. *)
  val add : map -> key -> value -> bool

  (**[add_absent m x v] inserts the key [x] with value [v] into the map
     [m]. [x], or some equivalent key, must not already be present in [m].
     No result is returned. *)
  val add_absent : map -> key -> value -> unit

  (**[find_key_else_add m x] determines whether some key [y] that is
     equivalent to [x] is present in the map [m]. If so, [y] is returned.
     Otherwise, the key [x] with value [v] is inserted into the map [m],
     and [Not_found] is raised.

     [find_key_else_add m x v] is equivalent to
     [try find_key m x v with Not_found -> add_absent m x v; raise Not_found]. *)
  val find_key_else_add : map -> key -> value -> key

  (**If some key [y] that is equivalent to [x] is present in the map
     [m], then [remove m x] removes [y] from the map [m] and returns
     [y]. Otherwise, this call has no effect and raises [Not_found]. *)
  val remove : map -> key -> key

  type population = int

  (**[population m] returns the number of inhabitants of the map [m]. *)
  val population : map -> population

  (**[cleanup m] cleans up the internal representation of the map [m] by
     freeing the space occupied by any previously removed keys. If many
     keys have been recently removed, this can free up some space and
     delay the need to grow the map's internal data arrays. *)
  val cleanup : map -> unit

  (**[clear m] empties the map [m]. The internal data arrays are retained,
     and are erased. The time complexity of this operation is linear in
     the size of the internal data arrays. *)
  val clear : map -> unit

  (**[reset m] empties the map [m]. The internal data arrays are abandoned.
     The time complexity of this operation is constant. *)
  val reset : map -> unit

  (**[copy m] returns a new map whose key-value bindings are those of [m].
     Its time complexity is linear in the size of the internal data arrays. *)
  val copy : map -> map

  (**[foreach_key f m] applies the user-supplied function [f] in turn to
     each key [x] in the map [m]. *)
  val foreach_key : (key -> unit) -> map -> unit

  (**[foreach_key_value f m] applies the user-supplied function [f] in
     turn to each pair of a key [x] and value [v] in the map [m]. *)
  val foreach_key_value : (key -> value -> unit) -> map -> unit

  (**[iter] is a synonym for [foreach_key_value]. *)
  val iter : (key -> value -> unit) -> map -> unit

  (**[show f m] returns a textual representation of the map [m]. The
     user-supplied function [f] is used to obtain a textual representation
     of each key. *)
  val show : (key -> string) -> map -> string

  (**[statistics m] returns a string of information about the population,
     capacity and occupancy of the map [m]. *)
  val statistics : map -> string

  (**/**)
  (* In debug builds, [check m] checks that the map's internal invariant
     holds. In release builds, [check m] has no effect. *)
  val check : map -> unit

end
