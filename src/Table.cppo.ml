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

(* This code can implement either a hash set or a hash map. We refer to this
   data structure in a neutral way as a "table". *)

(* If the table is a set, then we refer to the set's elements as keys. *)

(* If the table is a map, then we refer to the elements of the domain as
   keys, and we refer to the elements of the codomain as values. *)

#include "Signatures.cppo.ml"

module[@inline] Make_
(H : HashedType)
(S : SENTINELS with type t = H.t)
(K : ARRAY with type element = H.t)
#ifdef ENABLE_MAP
(V : sig include ARRAY val empty : t end)
#endif
= struct
open H
open S

type key =
  K.element

#ifdef ENABLE_MAP
type value =
  V.element
#endif

(* Although [equal] is traditionally named [equal], it is really
   an equivalence test. We rename it to [equiv] internally. *)

let equiv : key -> key -> bool =
  equal

(* [ov] stands for nothing if the table is a set,
    and stands for [v] if the table is a map. *)

(* [ovalue] stands for nothing if the table is a set,
   and stands for [value] if the table is a map. *)

#ifdef ENABLE_MAP
#define ov (v : value)
#define ovalue value
#else
#define ov
#define ovalue
#endif

(* -------------------------------------------------------------------------- *)

(* In the main [key] array, the content of each slot can be:

   + [void],   an empty slot;
   + [tomb],   a slot that was once occupied, but is now empty; or
   + [x],      a slot that currently contains the key [x]. *)

(* The difference between [void] and [tomb] is that [void] stops a search,
   whereas [tomb] does not. In other words, when searching linearly for a
   key [x], if an empty slot is encountered, then the search stops, as the
   data structure's invariant guarantees that [x] cannot appear beyond this
   empty slot; whereas if a tombstone is encountered, then the search
   continues, as [x] could appear beyond this tombstone. In other words, we
   maintain the following invariant: if [x] is in the table then it must
   appear between the index [start s x] and the first [void] slot. *)

(* Furthermore (this is optional), we maintain the invariant that a [tomb]
   slot is never followed with a [void] slot. To achieve this, in [remove],
   if the key that is being removed is followed with [void], then this key
   and all preceding tombstones are overwritten with [void]. This makes
   [remove] more costly but allows us to maintain a lower occupancy. *)

let forbid_tomb_void =
  true

(* Instead of using an algebraic data type, as follows:

     type content =
       | Void
       | Tomb
       | Key of key

   we represent [void] and [tomb] as two sentinels, that is, two special
   keys that the user is not allowed to insert into the table. This allows
   us to allocate fewer memory blocks and to use just an array of keys.

   We assume that a sentinel can be recognized using [==]. *)

type content =
  key

let[@inline] is_sentinel (c : content) =
  c == void || c == tomb

let[@inline] is_not_sentinel (c : content) =
  not (is_sentinel c)

(* A table is represented as follows. *)

type table = {
  (* The number of keys in the [key] array. *)
  mutable population : int;
  (* The number of keys and tombstones in the [key] array. *)
  mutable occupation : int;
  (* The capacity of the [key] array, minus one. *)
  mutable mask       : int;
  (* The key array. The length of this array is a power of two. *)
  mutable key        : K.t;
  #ifdef ENABLE_MAP
  (* The value array. If [occupation] is nonzero, then the key array and
     the value array have equal lengths. Otherwise, the value array can
     have zero length. (It is lazily allocated.) *)
  mutable value      : V.t;
  #endif
}

(* A hash code is an arbitrary integer. *)

type hash =
  int

(* An index into the [key] array. *)

type index =
  int

(* An array size. *)

type capacity =
  int

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

(* The definition of occupancy is based on [s.occupation], which counts both
   empty slots and tombstones. This is required to ensure that every linear
   search terminates.

   Indeed, imagine what could happen if occupancy counted empty slots only.
   Imagine that the [key] array is filled with tombstones. Then, occupancy
   would be zero, yet every search would diverge, as it would never find an
   empty slot. *)

let[@inline] population (s : table) =
  s.population

let[@inline] occupation (s : table) =
  s.occupation

let[@inline] capacity (s : table) : capacity =
  K.length s.key

let[@inline] occupancy (s : table) : float =
  float (occupation s) /. float (capacity s)

(* -------------------------------------------------------------------------- *)

(* [index s h] converts the hash code [h] to an index into the [key] array. *)

let[@inline] index (s : table) (h : hash) : index =
  (* Because the length of the [key] array is a power of two,
     the desired index can be computed by keeping just the least
     significant bits of the hash code [h], as follows. *)
  h land s.mask

(* [start s x] is the index where a search for [x] begins. *)

let[@inline] start (s : table) (x : key) : index =
  index s (hash x)

(* [next s i] increments the index [i] into the [key] array,
   while handling wrap-around. *)

let[@inline] next (s : table) (i : index) : index =
  (i + 1) land s.mask

(* [prev s i] decrements the index [i] into the [key] array,
   while handling wrap-around. *)

let[@inline] prev (s : table) (i : index) : index =
  (i - 1) land s.mask

(* [is_index s i] checks that [i] is valid index into the [key ]array. *)

let[@inline] is_index (s : table) (i : index) : bool =
  0 <= i && i < capacity s

(* -------------------------------------------------------------------------- *)

(* The functions [is_power_of_two] and [check] are used only during testing.  *)

let rec is_power_of_two c =
  c = 1 || is_power_of_two (c / 2)

let check s =
  assert begin
    (* The table's capacity is a power of two. *)
    let capacity = capacity s in
    assert (0 < capacity);
    assert (is_power_of_two capacity);
    (* [s.mask] is [capacity - 1]. *)
    assert (s.mask = capacity - 1);
    (* The table's population and occupation cannot exceed its capacity. *)
    assert (0 <= s.population && s.population <= capacity);
    assert (0 <= s.occupation && s.occupation <= capacity);
    (* The table's population, [s.population], is the number of non-sentinel
       slots in the [key] array. The table's occupation, [s.occupation], is
       the number of non-void slots in the [key] array. *)
    let pop, occ = ref 0, ref 0 in
    for k = 0 to capacity - 1 do
      let content = K.unsafe_get s.key k in
      if content == void then
        ()
      else if content == tomb then begin
        incr occ;
        if forbid_tomb_void then
          (* [tomb] is never followed with [void]. *)
          assert (K.unsafe_get s.key (next s k) != void)
      end
      else begin
        incr occ; incr pop
      end
    done;
    assert (s.population = !pop);
    assert (s.occupation = !occ);
    (* The [value] array either has length zero or has the same length
       as the [key] array. (It is lazily allocated.) If the population
       is nonzero then both arrays must have the same length. *)
    #ifdef ENABLE_MAP
    assert (V.length s.value = 0 || V.length s.value = capacity);
    assert (    s.occupation = 0 || V.length s.value = capacity);
    #endif
    true
  end

(* -------------------------------------------------------------------------- *)

(* Two parameters: initial capacity and maximal occupancy. *)

let initial_capacity =
  8

(* To avoid floating-point computations, we express [max_occupancy] as an
   integer value, which we multiply by 1/128. *)

let max_occupancy =
  105 (* 105/128 = 0.82 *)

(* [crowded] determines whether the table's maximum occupancy rate has
   been exceeded. It is paremeterized by the table's current occupation
   and capacity. *)

let[@inline] crowded occupation capacity =
  128 * occupation > max_occupancy * capacity

(* [full] determines whether the table is full. A table is full when
   it contains no [void] slots, that is, when its occupation equals
   its capacity. *)

let[@inline] full occupation capacity =
  occupation = capacity

(* [crowded_or_full] is the disjunction of the previous two criteria.
   See [possibly_grow] for an explanation of why we use two separate
   criteria. *)

let[@inline] crowded_or_full occupation capacity =
  crowded occupation capacity || full occupation capacity

(* -------------------------------------------------------------------------- *)

(* The value array is lazily allocated. *)

#ifdef ENABLE_MAP

let value_array_is_allocated s =
  V.length s.value > 0

let[@inline] allocate_value_array s (dummy : value) =
  s.value <- V.make (capacity s) dummy

let[@inline] possibly_allocate_value_array s (dummy : value) =
  if V.length s.value = 0 then
    allocate_value_array s dummy

let[@inline] get_value (s : table) (j : index) =
  assert (is_index s j);
  assert (value_array_is_allocated s);
  V.unsafe_get s.value j

let[@inline] set_value (s : table) (j : index) (v : value) =
  assert (is_index s j);
  assert (value_array_is_allocated s);
  V.unsafe_set s.value j v

#endif

(* When [MAP] is defined, [POSSIBLY_ALLOCATE_VALUE_ARRAY] expands to
   [possibly_allocate_value_array s v]. Otherwise, it expands to nothing. *)

#def POSSIBLY_ALLOCATE_VALUE_ARRAY
  #ifdef ENABLE_MAP
    possibly_allocate_value_array s v
  #else
    ()
  #endif
#enddef

(* -------------------------------------------------------------------------- *)

(* [zap s j] zaps slot [j].

   Slot [j] must contain a key, as opposed to a sentinel. *)

(* To zap a slot means to overwrite this slot with [tomb] or [void]. *)

(* Overwriting a slot with [void] is correct only if the next slot is
   [void] already. *)

(* [s.population] is not affected. *)

(* [s.occupation] is decreased by the number of [void] slots that we create. *)

(* The [value] array is unaffected. We tolerate garbage in it. *)

let zap s j =
  assert (is_index s j);
  assert (is_not_sentinel (K.unsafe_get s.key j));
  (* Test whether the next slot is void. *)
  if forbid_tomb_void && K.unsafe_get s.key (next s j) == void then begin
    (* The next slot is void. In order to maintain the invariant
       that [tomb] is never followed with [void], we must replace
       [x], as well as all previous tombstones, with [void]. *)
    K.unsafe_set s.key j void;
    let k = ref (prev s j) in
    let count = ref 1 in
    while K.unsafe_get s.key !k == tomb do
      K.unsafe_set s.key !k void;
      k := prev s !k;
      count := !count + 1
    done;
    (* [s.occupation] is decreased by the number of [void] slots
       that we have been able to recreate. *)
    s.occupation <- s.occupation - !count
  end
  else begin
    (* The next slot is not void, or we do not forbid [tomb] followed
       with [void]. Write a tombstone at index [j]. *)
    K.unsafe_set s.key j tomb
    (* [s.occupation] is unchanged. *)
  end

(* -------------------------------------------------------------------------- *)

(* A template for a search function. *)

(* The macro [SEARCH_WITH_ACCU(SELF, ACCU, ABSENT, PRESENT, ACCU')]
   defines a search function.

   [SELF] is the name of the function.

   The parameters of this function are:
   - the table [s];
   - the desired key [x];
   - the current index [j] of the search;
   - the optional accumulator [ACCU].

   [ACCU] is a formal parameter, and can be empty.

   [ABSENT] is executed if the key [x] is absent (not found).
   This code can refer to [s], [x], [j], [ACCU].

   [PRESENT] is executed if a key [y] that is equivalent to [x] is found.
   This code can refer to [s], [x], [j], [ACCU], and [y].

   The updated accumulator [ACCU'] is passed to the recursive calls.
   This code can refer to [s], [x], [j], [ACCU]. *)

#def SEARCH_WITH_ACCU(SELF, ACCU, ABSENT, PRESENT, ACCU')

let rec SELF (s : table) (x : key) (j : int) ACCU =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* This slot is void. *)
    (* [x] is not in the table. *)
    (ABSENT)
  else if c == tomb then
    (* This slot is a tombstone. *)
    (* [x] might appear in the table beyond this tombstone. *)
    (* Skip this slot and continue searching. *)
    SELF s x (next s j) ACCU'
  else
    let y = c in
    if equiv x y then
      (* We have found a key [y] that is equivalent to [x]. *)
      (PRESENT)
    else
      (* Skip this slot and continue searching. *)
      SELF s x (next s j) ACCU'

#enddef

(* The macro [SEARCH(SELF, ABSENT, PRESENT)]
   defines a search function without an accumulator. *)

#def SEARCH(SELF, ABSENT, PRESENT)
SEARCH_WITH_ACCU(SELF,, ABSENT, PRESENT,)
#enddef

(* -------------------------------------------------------------------------- *)

(* Lookup functions. *)

(* These functions perform read-only access to the table, so they can safely
   be called by several concurrent threads. This is documented. Therefore,
   these functions *must not* use the search template [SEARCH2], which moves
   elements within the table, therefore performs write accesses. *)

(* [mem] determines whether the key [x] (or some equivalent key) is present
   in the table. It returns a Boolean result. *)

SEARCH(mem,
  false,
  true
)

(* [find_key] is analogous to [mem], but returns the key [y] that is found,
   and raises an exception if no key that is equivalent to [x] is found. *)

SEARCH(find_key,
  raise Not_found,
  y
)

(* [find_value] is analogous to [find_key], but returns the value associated
   with the key [y], instead of the key [y] itself. *)

#ifdef ENABLE_MAP

SEARCH(find_value,
  raise Not_found,
  get_value s j
)

#endif

(* [length] is analogous to [mem], but measures the length of the linear
   scan that is required to find [x]. It is used by [statistics]. *)

SEARCH_WITH_ACCU(length, accu,
  accu,
  accu,
accu + 1)

(* -------------------------------------------------------------------------- *)

(* Deletion functions. *)

(* [remove] searches for the key [x] (or some equivalent key). If a key
   [y] is found, then this key is removed. Otherwise, nothing happens. *)
(* The fields [s.population] and [s.occupation] are updated. *)
(* The [value] array is unaffected. We tolerate garbage in it. *)

SEARCH(remove,
  (),
  (* If a key [y] that is equivalent to [x] is found at index [j],
     then we decrease the population, zap slot [j], and return [y]. *)
  s.population <- s.population - 1; zap s j
)

(* [find_key_and_remove] is analogous to [remove], except the key [y]
   is returned (if such a key is found). Otherwise, an exception is
   raised. *)

SEARCH(find_key_and_remove,
  raise Not_found,
  (* If a key [y] that is equivalent to [x] is found at index [j],
     then we decrease the population, zap slot [j], and return [y]. *)
  s.population <- s.population - 1; zap s j; y
)

#ifdef ENABLE_MAP

SEARCH(find_value_and_remove,
  raise Not_found,
  (* If a key is found at index [j], then we decrease the population,
     read the value [v] at index [j], zap slot [j], and return [v]. *)
  s.population <- s.population - 1;
  let v = get_value s j in zap s j; v
)

#endif

(* -------------------------------------------------------------------------- *)

(* [choose s j] searches the table linearly, from index [j], and returns
   the first key that it finds. *)

(* The table must be nonempty; that is, its population must be nonzero. *)

let rec choose (s : table) (j : int) : key =
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void || c == tomb then
    (* Skip this slot and continue searching. *)
    choose s (next s j)
  else
    let y = c in
    (* Return this key. *)
    y

(* -------------------------------------------------------------------------- *)

(* A template for a search function that remembers passing a tombstone,
   and comes back to this tombstone once the search ends. *)

(* Two functions are generated. They correspond to the two states of a
   simple state machine. In the initial state, which corresponds to the
   main function, no tombstone has been encountered yet. In the final
   state, which corresponds to the auxiliary function, a tombstone has
   been encountered, and its index [t] has been recorded. *)

(* In the final state, if the desired key is not found, then the search
   moves back to slot [t]. That is, [j] is set to [t] before [ABSENT] is
   executed. *)

(* In the final state, if the desired key is found, then the key [y] and
   its value are moved (copied) back to slot [t], and [j] is set to [t]
   before [PRESENT] is executed. *)

(* In either case, to an external observer, everything appears to work
   just as if the search had terminated at index [j]. The observer does
   not see that the search has gone further right and come back left. *)

(* The macro [SEARCH2(SELF, ABSENT, PRESENT)] defines a search function.

   [SELF] is the name of the main function.

   The parameters of this function are:
   - the table [s];
   - the desired key [x];
   - an optional value [ov];
   - the current index [j] of the search.

   [ABSENT] is executed if the key [x] is absent (not found).
   This code can refer to [s], [x], [ov], [j].
   This code can pretend that slot [j] in the [key] array contains [void]
   and *must* overwrite this slot with a key.
   This code must not update [s.occupation]; this is taken care of.
   This code can assume that the [value] array has been allocated.

   [PRESENT] is returned if a key [y] that is equivalent to [x] is found.
   This code can refer to [s], [x], [ov], [j], and [y].

   [CONCAT(SELF, _aux)] is the name of the auxiliary function.

   The parameters of this function are:
   - the table [s];
   - the desired key [x];
   - an optional value [ov];
   - the index [t] of the tombstone that has been encountered;
   - the current index [j] of the search. *)

#def SEARCH2(SELF, ABSENT, PRESENT)

let rec SELF (s : table) (x : key) ov (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    (* This slot is void. *)
    (* [x] is not in the table. *)
    (* Update [s.occupation]. *)
    s.occupation <- s.occupation + 1;
    POSSIBLY_ALLOCATE_VALUE_ARRAY;
    ABSENT
  end
  else if c == tomb then
    (* This slot is a tombstone. *)
    (* [x] might appear in the table beyond this tombstone. *)
    (* Skip this slot and continue searching. *)
    (* Switch to the second state, where a tombstone at index [t]
       has been encountered. *)
    let t = j in
    CONCAT(SELF, _aux) s x ov t (next s j)
  else
    let y = c in
    if equiv x y then
      (* We have found a key [y] that is equivalent to [x]. *)
      (PRESENT)
    else
      (* Skip this slot and continue searching. *)
      SELF s x ov (next s j)

and CONCAT(SELF, _aux) (s : table) (x : key) ov (t : int) (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s t);
  assert (K.unsafe_get s.key t == tomb);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* This slot is void. *)
    (* [x] is not in the table. *)
    (* Set [j] back down to [t]. *)
    (* Even though slot [t] still contains [tomb], it is now logically
       considered void. This slot will be overwritten by [ABSENT], so
       there is no need to actually write [void] into it. Also, there
       is no need to update [s.occupation], as [ABSENT] will overwrite
       the tombstone with a key. *)
    (* As we have seen a tombstone, the [value] array must be allocated. *)
    (let j = t in ABSENT)
  else if c == tomb then
    (* This slot is a tombstone. *)
    (* [x] might appear in the table beyond this tombstone. *)
    (* Skip this slot and continue searching. *)
    CONCAT(SELF, _aux) s x ov t (next s j)
  else
    let y = c in
    if equiv x y then begin
      (* We have found a key [y] that is equivalent to [x]. *)
      (* Move the key [y], and its value, from slot [j] down to slot [t],
         thereby overwriting the tombstone at [t]. Then, zap slot [j].
         Thus, the next search for [x] or [y] will be faster. Furthermore,
         this can turn one or more occupied slots back into void slots. *)
      K.unsafe_set s.key t y;
      #ifdef ENABLE_MAP
      set_value s t (get_value s j);
      #endif
      zap s j;
      (* Move the index [j] back down to [t], and execute [PRESENT]. *)
      let j = t in PRESENT
    end
    else
      (* Skip this slot and continue searching. *)
      CONCAT(SELF, _aux) s x ov t (next s j)

#enddef

(* -------------------------------------------------------------------------- *)

(* The macro [WRITE] writes key [x] and value [v] at index [j]. *)

(* It assumes that the [value] array is allocated. *)

#def WRITE
  K.unsafe_set s.key j x
  #ifdef ENABLE_MAP
  ;set_value s j v
  #endif
#enddef

(* The macro [WRITE_AND_POPULATE] writes key [x] and value [v] at index [j]
   and increments [s.population]. *)

(* It assumes that the [value] array is allocated. *)

#def WRITE_AND_POPULATE
  s.population <- s.population + 1;
  WRITE
#enddef

(* -------------------------------------------------------------------------- *)

(* Insertion. *)

(* [add_if_absent] searches for the key [x] and inserts it if it is absent. *)
(* The Boolean result indicates whether [x] was inserted. *)
(* The fields [s.population] and [s.occupation] are updated. *)

(* If the table is a map, then the user supplies a value [v]
   in addition to the key [x], and this value is written to
   the [value] array. *)

SEARCH2(add_if_absent,
  (* If [x] is not found, it is inserted at [j], and [true] is returned. *)
  WRITE_AND_POPULATE; true,
  (* If [x] or an equivalent key is found, [false] is returned. *)
  ignore j; false
)

(* In [add_if_absent], in case a tombstone is encountered, one might be
   tempted to always overwrite this tombstone with [x], then use [remove] to
   find and remove any key [y] that is equivalent to [x] and that is already
   a member of the table. However, this does not work. If the table already
   contains a key [y] that is equivalent to [x], then [add_if_absent] is
   expected to leave [y] in the table; it must not replace [y] with [x]. *)

(* [find_key_else_add] searches for [x] and inserts it if it is absent. *)
(* If [x] was absent then [Not_found] is raised after [x] is inserted. *)
(* If a key [y] is found then [y] is returned. *)

SEARCH2(find_key_else_add,
  (* If [x] is not found, it is inserted at [j], and [Not_found] is raised. *)
  WRITE_AND_POPULATE; raise Not_found,
  (* If a key [y] that is equivalent to [x] is found, [y] is returned. *)
  ignore j; y
)

(* [find_value_else_add] searches for [x] and inserts it if it is absent. *)
(* If [x] was absent then [Not_found] is raised after [x] is inserted. *)
(* If a key is found then the corresponding value is returned. *)

#ifdef ENABLE_MAP

SEARCH2(find_value_else_add,
  (* If [x] is not found, it is inserted at [j], and [Not_found] is raised. *)
  WRITE_AND_POPULATE; raise Not_found,
  (* If a key [y] that is equivalent to [x] is found, then the value
     associated with [y] is returned. *)
  get_value s j
)

#endif

(* [replace] always inserts the key [x] with value [v], possibly overwriting
   a previous key and value. Thus, if no key that is equivalent to [x]
   exists, then [x] and [v] are inserted; otherwise, the previous key and
   value are replaced with [x] and [v]. *)

SEARCH2(replace,
  (* If [x] is not found, it is inserted at [j]. *)
  WRITE_AND_POPULATE; true,
  (* If [x] or an equivalent key is found,
     [x] and the value [v] are written at [j]. *)
  WRITE; false
)

(* -------------------------------------------------------------------------- *)

(* Insertion: [add_absent]. *)

(* A special case of [add_if_absent], where we assume that [x] is not in the
   table. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* The fields [s.population] and [s.occupation] are updated. *)

let rec add_absent (s : table) (x : key) ov (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    s.occupation <- s.occupation + 1;
    POSSIBLY_ALLOCATE_VALUE_ARRAY;
    WRITE_AND_POPULATE
  end
  else if c == tomb then
    (* Because [x] is not in the table, it can be safely inserted here,
       by overwriting this tombstone. *)
    (WRITE_AND_POPULATE)
    (* [s.occupation] is unchanged. *)
  else
    let y = c in
    (* [x] is not in the table. *)
    assert (not (equiv x y));
    (* Skip this slot and continue searching. *)
    add_absent s x ov (next s j)

(* -------------------------------------------------------------------------- *)

(* [add_absent_no_updates] is a special case of [add_if_absent], where:

   + we assume that [x] is not in the table;
   + we assume that there are no tombstones;
   + the fields [s.population] and [s.occupation] are NOT updated. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* This auxiliary function is used by [resize] and by [elim]. *)

let rec add_absent_no_updates (s : table) (x : key) ov (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  assert (c != tomb);
  if c == void then begin
    POSSIBLY_ALLOCATE_VALUE_ARRAY;
    WRITE
  end
  else
    let y = c in
    assert (not (equiv x y));
    add_absent_no_updates s x ov (next s j)

(* -------------------------------------------------------------------------- *)

(* [resize s new_capacity] allocates a new key array whose capacity is
   [new_capacity]. Then, it copies the content of the old key array to
   the new one. All tombstones disappear in the process. *)

(* [new_capacity] must be a power of two, and must be large enough to
   ensure that (once all tombstones have disappeared) the table is not
   crowded. *)

(* The [value] array is resized in a similar way. *)

let resize (s : table) (new_capacity : capacity) =
  assert (is_power_of_two new_capacity);
  assert (not (crowded_or_full s.population new_capacity));
  let old_key = s.key in
  #ifdef ENABLE_MAP
  let old_value = s.value in
  #endif
  let old_capacity = capacity s in
  s.mask <- new_capacity - 1;
  (* Resize the [key] array. *)
  s.key <- K.make new_capacity void;
  (* Resize the [value] array, unless its length is zero. *)
  #ifdef ENABLE_MAP
  if V.length old_value > 0 then begin
    assert (old_capacity > 0);
    assert (V.length old_value = old_capacity);
    let dummy = V.unsafe_get old_value 0 in
    allocate_value_array s dummy
  end;
  #endif
  (* At this point, [s] is a valid empty table, except for the [population]
     and [occupation] fields. *)
  (* Every key of the old key array must now be inserted into [s]. Each
     insertion operation inserts a new key (one that is not already
     present), and no tombstones can be encountered. Also, the [population]
     and [occupation] fields need not be updated. Thus, [add_absent_no_updates]
     is used. *)
  for k = 0 to old_capacity - 1 do
    let c = K.unsafe_get old_key k in
    if is_not_sentinel c then
      let x = c in
      #ifdef ENABLE_MAP
      assert (s.population > 0);
      assert (V.length old_value = old_capacity);
      let v = V.unsafe_get old_value k in
      #endif
      add_absent_no_updates s x ov (start s x)
  done;
  (* The population is unchanged. There are no tombstones any more,
     so [s.occupation] now coincides with [s.population]. *)
  s.occupation <- s.population

(* -------------------------------------------------------------------------- *)

(* Eliminating tombstones, in place, in the [key] array. *)

(* Roughly speaking, all tombstones are turned into [void] slots, and all
   keys that used to follow a tombstone must be relocated. *)

(* We use a state machine with 3 states, as follows:

   - In state [init], we have not yet encountered a void slot; we skip all
     keys and tombstones until we encounter one.

   - In state [void], we have encountered a void slot, followed with zero,
     one, or more keys. These keys can remain where they are; we skip them.

   - In state [tomb], we have encountered a run of one or more tombstones,
     which we have changed into void slots. We have then encountered zero,
     one, or more keys, which we have relocated.

   When we transition out of the initial state, we record the current index;
   this is the [origin] index. In every state other than [init], if we
   detect that we have reached [origin] again, after scanning the entire
   circular array, then we stop. *)

let rec elim_init (s : table) (j : index) : unit =
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* Switch from state [init] to state [void]. *)
    let origin = j in
    elim_void s origin (next s j)
  else
    (* Continue in state [init]. *)
    elim_init s (next s j)

and elim_void (s : table) (origin : index) (j : index) : unit =
  assert (is_index s origin);
  assert (K.unsafe_get s.key origin == void);
  assert (is_index s j);
  if origin <> j then
    let c = K.unsafe_get s.key j in
    if c == tomb then
      (* Overwrite this tombstone and switch to state [tomb]. *)
      write_elim_tomb s origin j
    else
      (* Continue in state [void]. *)
      elim_void s origin (next s j)

and write_elim_tomb (s : table) (origin : index) (j : index) : unit =
  assert (is_index s origin);
  assert (K.unsafe_get s.key origin == void);
  assert (is_index s j);
  assert (K.unsafe_get s.key j == tomb);
  (* Overwrite the tombstone at [j] with [void]. *)
  K.unsafe_set s.key j void;
  (* Continue at the next slot in state [tomb]. *)
  elim_tomb s origin (next s j)

and elim_tomb (s : table) (origin : index) (j : index) : unit =
  assert (is_index s origin);
  assert (K.unsafe_get s.key origin == void);
  assert (is_index s j);
  if origin <> j then
    let c = K.unsafe_get s.key j in
    if c == void then
      (* Switch to state [void]. *)
      elim_void s origin (next s j)
    else if c == tomb then
      (* Overwrite this tombstone and continue in state [tomb]. *)
      write_elim_tomb s origin j
    else
      (* The key [x] must be relocated. *)
      let x = c in
      (* Overwrite this slot, effectively removing [x] from the table,
         without updating [s.population] or [s.occupation]. *)
      K.unsafe_set s.key j void;
      (* Read the value [v] that is associated with [x]. *)
      #ifdef ENABLE_MAP
      let v = get_value s j in
      #endif
      (* Now relocate this key-value pair: insert key [x] with value [v]. *)
      (* This insertion reads and updates a part of the table that we have
         already scanned and where we have already eliminated all tombstones,
         between [origin] (excluded) and [j] (included). *)
      assert (
         origin < j && origin < start s x && start s x <= j
      || j < origin && (origin < start s x || start s x <= j)
      );
      add_absent_no_updates s x ov (start s x);
      (* Continue in state [tomb]. *)
      elim_tomb s origin (next s j)

(* [elim] is the main entry point for the above state machine. *)

(* [elim s] eliminates all tombstones, in linear time, in place. *)

(* The cost of [elim s] is the cost of scanning the entire [key] array plus
   the cost of relocating (re-inserting) all of the keys that follow a
   tombstone. The keys that follow a void slot are not relocated, so do not
   contribute to the second term in this sum. *)

(* A much simpler way of implementing [elim] would be [resize s (capacity s)],
   which relocates all keys into a fresh key array. This simpler way is less
   efficient because it requires allocating a fresh array and relocating *all*
   keys. *)

let[@inline] elim (s : table) =
  (* Execute the state machine. We start at index 0, but one could start
     anywhere. *)
  let j = 0 in
  elim_init s j;
  (* All tombstones are now gone. *)
  s.occupation <- s.population

(* -------------------------------------------------------------------------- *)

(* Growing and shrinking a table. *)

let[@inline] possibly_grow (s : table) =
  (* If the maximum occupancy is now exceeded, then the capacity of the [key]
     array must be increased. This is heuristic: keeping occupancy low allows
     us to keep the expected length of a linear search low. *)
  (* Furthermore, to ensure that every linear search terminates, one must
     guarantee that there is always at least one [void] slot in the [key]
     array. This is not heuristic: it is a hard requirement. *)
  (* We could enforce both conditions at once by imposing the constraint
     [max_occupancy + 1/capacity <= 1]. Then, the maximum occupancy check,
     alone, would ensure the existence of at least one [void] slot. We prefer
     to remove this constraint, at the cost of performing two tests. *)
  let o = occupation s
  and c = capacity s in
  if crowded_or_full o c then
    (* Double the capacity of the [key] array. *)
    resize s (2 * c);
  (* There must always remain at least one empty slot. Otherwise, searches
     would diverge. *)
  assert (s.occupation < capacity s)

(* [possibly_shrink s new_capacity] shrinks the capacity of the table
   to [new_capacity] or to a lower capacity. *)

(* We never shrink a table below [initial_capacity], as that would be
   counter-productive. *)

(* To determine whether the capacity [new_capacity] can be safely divided
   by two, we use [crowded_or_full]. We apply this test to [s.population],
   as opposed to [s.occupation], because if the table is shrunk, then all
   tombstones will disappear. Hence, the current tombstones should not be
   taken into account when determining whether the shrunk table would be
   crowded. *)

let rec possibly_shrink (s : table) (new_capacity : capacity) =
  assert (is_power_of_two new_capacity);
  assert (initial_capacity <= new_capacity);
  assert (new_capacity <= capacity s);
  if new_capacity = initial_capacity
  || crowded_or_full s.population (new_capacity / 2) then begin
    (* The capacity cannot be divided by two. If it is less than the
       current capacity, then the table must be resized. Otherwise,
       there is nothing to do. *)
    if new_capacity < capacity s then
      resize s new_capacity
  end
  else
    (* The capacity can be divided by two. *)
    possibly_shrink s (new_capacity / 2)

(* -------------------------------------------------------------------------- *)

(* Public functions. *)

let create () =
  let capacity = initial_capacity in
  let population = 0
  and occupation = 0
  and mask = capacity - 1
  and key = K.make capacity void
  #ifdef ENABLE_MAP
  and value = V.empty
  #endif
  in
  { population; occupation; mask; key; ovalue }

let[@inline] validate (x : key) =
  assert (is_not_sentinel x)
    (* We use an assertion that is erased in release mode.
       If we wanted this module to be more defensive, we
       could keep a defensive test in release mode. *)

let[@inline] mem (s : table) (x : key) : bool =
  validate x;
  mem s x (start s x)

let[@inline] find_key (s : table) (x : key) : key =
  validate x;
  find_key s x (start s x)

#ifdef ENABLE_MAP

let[@inline] find_value (s : table) (x : key) : value =
  validate x;
  find_value s x (start s x)

#endif

let choose (s : table) : key =
  if population s = 0 then
    raise Not_found
  else
    (* Pick an index at random, and search from there. *)
    let j = Random.int (capacity s) in
    choose s j

let[@inline] length (s : table) (x : key) : int =
  (* No need to validate [x]; this function is private. *)
  length s x (start s x) 0

let[@inline] cleanup (s : table) =
  (* First, shrink the table, if its occupation is sufficiently low. *)
  possibly_shrink s (capacity s);
  (* Then, if the table contains any tombstones (which can be the case
     only if the table was not shrunk above), scan the [key] array and
     eliminate all tombstones. *)
  if s.occupation > s.population then
    elim s

let add_if_absent (s : table) (x : key) ov : bool =
  validate x;
  let was_added = add_if_absent s x ov (start s x) in
  if was_added then possibly_grow s;
  was_added

let add_absent (s : table) (x : key) ov =
  validate x;
  add_absent s x ov (start s x);
  possibly_grow s

let find_key_else_add (s : table) (x : key) ov =
  validate x;
  try
    find_key_else_add s x ov (start s x)
  with Not_found as e ->
    possibly_grow s;
    raise e

#ifdef ENABLE_MAP

let find_value_else_add (s : table) (x : key) v =
  validate x;
  try
    find_value_else_add s x v (start s x)
  with Not_found as e ->
    possibly_grow s;
    raise e

#endif

let replace (s : table) (x : key) ov : bool =
  validate x;
  let was_added = replace s x ov (start s x) in
  if was_added then possibly_grow s;
  was_added

let[@inline] remove (s : table) (x : key) : unit =
  validate x;
  remove s x (start s x)

let[@inline] find_key_and_remove (s : table) (x : key) : key =
  validate x;
  find_key_and_remove s x (start s x)

#ifdef ENABLE_MAP

let[@inline] find_value_and_remove (s : table) (x : key) : value =
  validate x;
  find_value_and_remove s x (start s x)

#endif

let clear (s : table) =
  s.population <- 0;
  s.occupation <- 0;
  K.fill s.key 0 (capacity s) void
  (* The [value] array is unaffected. We tolerate garbage in it. *)

let reset (s : table) =
  let capacity = initial_capacity in
  let population = 0
  and occupation = 0
  and mask = capacity - 1
  and key = K.make capacity void in
  s.population <- population;
  s.occupation <- occupation;
  s.mask <- mask;
  s.key <- key;
  #ifdef ENABLE_MAP
  s.value <- V.empty;
  #endif
  ()

(* One might ask whether [copy] should return an identical copy or
   construct a fresh hash set that does not contain any tombstones. We
   choose the first option, because it is simpler and more efficient;
   it does not require hashing. *)

let copy (s : table) : table =
  { s with
    key = K.copy s.key
  #ifdef ENABLE_MAP
  ; value = V.copy s.value
  #endif
  }

let foreach_key f (s : table) =
  if s.population > 0 then
    for i = 0 to K.length s.key - 1 do
      let c = K.unsafe_get s.key i in
      if is_not_sentinel c then
        let x = c in
        f x
    done

#ifdef ENABLE_MAP

let foreach_key_value f (s : table) =
  if s.population > 0 then
    for i = 0 to K.length s.key - 1 do
      let c = K.unsafe_get s.key i in
      if is_not_sentinel c then
        let x = c in
        let v = get_value s i in
        f x v
    done

#endif

#ifdef ENABLE_MAP

let show show_key show_value (s : table) =
  let b = Buffer.create 32 in
  Buffer.add_string b "{";
  let first = ref true in
  foreach_key_value (fun x v ->
    if not !first then Buffer.add_string b ", ";
    Buffer.add_string b (show_key x);
    Buffer.add_string b " ↦ ";
    Buffer.add_string b (show_value v);
    first := false
  ) s;
  Buffer.add_string b "}";
  Buffer.contents b

#else

let show show_key (s : table) =
  let b = Buffer.create 32 in
  Buffer.add_string b "{";
  let first = ref true in
  foreach_key (fun x ->
    if not !first then Buffer.add_string b ", ";
    Buffer.add_string b (show_key x);
    first := false
  ) s;
  Buffer.add_string b "}";
  Buffer.contents b

#endif

(* -------------------------------------------------------------------------- *)

(* Statistics. *)

(* An integer histogram is a multiset of integers, that is, a finite map of
   integer values to multiplicities. *)

module IntMap =
  Map.Make(Int)

type multiplicity = int

type histogram =
  multiplicity IntMap.t

let multiplicity l (h : histogram) : multiplicity =
  try IntMap.find l h with Not_found -> 0

let insert l (h : histogram) : histogram =
  IntMap.add l (multiplicity l h + 1) h

let histogram (s : table) : histogram =
  let h = ref IntMap.empty in
  foreach_key (fun x ->
    (* Measure the length [l] of the search for [x]. *)
    let l = length s x in
    (* Increment the multiplicity of [l] in the histogram. *)
    h := insert l !h
  ) s;
  !h

let average (h : histogram) : float =
  let num, denum = ref 0, ref 0 in
  IntMap.iter (fun l m ->
    num := !num + m * l;
    denum := !denum + m;
  ) h;
  float !num /. float !denum

open Printf

let statistics (s : table) : string =
  let b = Buffer.create 128 in
  bprintf b "Population: %9d\n" (population s);
  bprintf b "Tombstones: %9d\n" (occupation s - population s);
  bprintf b "Capacity  : %9d\n" (capacity s);
  bprintf b "Occupancy : %.3f\n" (occupancy s);
  let h = histogram s in
  bprintf b "Average search length: %.3f\n" (average h);
  bprintf b "Histogram:\n";
  IntMap.iter (fun l m ->
    bprintf b
      "  %9d keys have search length %3d.\n"
      m l
  ) h;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Final packaging. *)

(* Common names: *)

type t = table

let cardinal = population

let[@inline] is_empty s =
  cardinal s = 0

#ifdef ENABLE_MAP

(* [map]-specific names: *)

type map = table

let find = find_value
let iter = foreach_key_value

#else

(* [set]-specific names: *)

type element = key
type set = table

let find = find_key
let find_else_add = find_key_else_add
let find_and_remove = find_key_and_remove
let iter = foreach_key

#endif

end

(* -------------------------------------------------------------------------- *)

(* [MonoArray(X)] creates a copy of [Stdlib.Array] that is specialized for
   array elements of type [X.t]. *)

module[@inline] MonoArray
(X : sig type t end)
: sig
  include ARRAY with type element = X.t
#ifdef ENABLE_MAP
  val empty : t
#endif
end
= struct
  type element = X.t
  type t = element array
  let empty = [||]
  let make = Array.make
  let copy = Array.copy
  let length = Array.length
  let[@inline] unsafe_get (a : t) i = Array.unsafe_get a i
  let[@inline] unsafe_set (a : t) i x = Array.unsafe_set a i x
  let fill = Array.fill
end

(* -------------------------------------------------------------------------- *)

(* For people who want to apply the functor [_Make] to [Stdlib.Array] (twice),
   we propose a functor, [Make], that is easier to use. *)

module[@inline] Make
(H : HashedType)
(S : SENTINELS with type t = H.t)
#ifdef ENABLE_MAP
(V : sig type t end)
#endif
=
  Make_
    (H)
    (S)
    (MonoArray(H))
#ifdef ENABLE_MAP
    (MonoArray(V))
#endif
