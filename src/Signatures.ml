module type ARRAY = sig
  type element
  type t
  val make : int -> element -> t
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

module type S = sig

  (**The type of the elements of a set. *)
  type element

  (**The type of sets. A set always contains at most one element in each
     equivalence class: that is, [mem s x] and [mem s y] and [equiv x y]
     imply [x = y]. *)
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

     [find_else_add s x ] is equivalent to
     [try find s x with Not_found -> add_absent s x; raise Not_found]. *)
  val find_else_add : set -> element -> element

  (**If some element [y] that is equivalent to [x] is a member of the set
     [s], then [remove s x] removes [y] from the set [s] and returns
     [y]. Otherwise, this call has no effect and raises [Not_found]. *)
  val remove : set -> element -> element

  type population = int

  (**[population s] returns the number of inhabitants of the set [s]. *)
  val population : set -> population

  (**[cleanup s] cleans up the internal representation of the set by freeing
     the space occupied by any previously removed elements. If many elements
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
  val iter : (element -> unit) -> set -> unit

  (**[show f s] returns a textual representation of the members of
     the set [s]. The user-supplied function [f] is used to obtain a
     textual representation of each member. *)
  val show : (element -> string) -> set -> string

  (**[statistics s] returns a string of information about the population,
     capacity and occupancy of the set [s]. *)
  val statistics : set -> string

  (**/**)
  (* In debug builds, [check s] checks that the set's internal invariant
     holds. In release builds, [check s] has no effect. *)
  val check : set -> unit

end
