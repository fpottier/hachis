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

let[@inline] capacity (s : table) : capacity =
  K.length s.key

let[@inline] occupancy (s : table) : float =
  float (s.occupation) /. float (capacity s)

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

let[@inline] crowded s =
  128 * s.occupation > max_occupancy * capacity s

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

let[@inline] cautiously_set_value (s : table) (j : index) (v : value) =
  let dummy = v in
  possibly_allocate_value_array s dummy;
  set_value s j v

#endif

(* -------------------------------------------------------------------------- *)

(* Membership tests: [mem], [find_key], [find_value]. *)

(* We search for a key [x] in order to determine whether [x] (or some key
   that is equivalent to [x]) is present in the table. *)

(* [j] is the index that is currently under examination. *)

(* The Boolean result indicates whether [x] was found. *)

let rec mem (s : table) (x : key) (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* [x] is not in the table. *)
    false
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    mem s x (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have succeeded;
       otherwise, skip this slot and continue searching. *)
    equiv x y || mem s x (next s j)

(* [find_key] is analogous to [mem], but returns the key [y] that is found,
   and raises an exception if no key that is equivalent to [x] is found. *)

let rec find_key (s : table) (x : key) (j : int) : key =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* [x] is not in the table. *)
    raise Not_found
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    find_key s x (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then y else find_key s x (next s j)

#ifdef ENABLE_MAP

(* [find_value] is analogous to [find_key], but returns the value associated
   with the key [y], instead of the key [y] itself. *)

let rec find_value (s : table) (x : key) (j : int) : value =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* [x] is not in the table. *)
    raise Not_found
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    find_value s x (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then get_value s j else find_value s x (next s j)

#endif

(* [length] is analogous to [mem], but measures the length of the linear
   scan that is required to find [x]. It is used by [statistics]. *)

let rec length (s : table) (x : key) (j : int) (accu : int) : int =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* [x] is not in the table. *)
    accu
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    length s x (next s j) (accu + 1)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have succeeded;
       otherwise, skip this slot and continue searching. *)
    if equiv x y then accu else length s x (next s j) (accu + 1)

(* -------------------------------------------------------------------------- *)

(* [zap s j v] zaps slot [j] and returns [v].

   Slot [j] must contain a key, as opposed to a sentinel. *)

(* To zap a slot means to overwrite this slot with [tomb] or [void]. *)

(* Overwriting a slot with [void] is correct only if the next slot is
   [void] already. *)

(* [s.population] is not affected. *)

(* [s.occupation] is decreased by the number of [void] slots that we create. *)

(* The [value] array is unaffected. We tolerate garbage in it. *)

let zap s j v =
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
  end;
  v

(* -------------------------------------------------------------------------- *)

(* Deletion: [remove]. *)

(* We search for a key [x] and remove it if it is present. *)

(* The Boolean result indicates whether [x] was found and removed. *)

(* The fields [s.population] and [s.occupation] are updated. *)

(* The [value] array is unaffected. We tolerate garbage in it. *)

let rec remove (s : table) (x : key) (j : int) : key =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then
    (* [x] is not in the table. *)
    raise Not_found
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    remove s x (next s j)
  else
    let y = c in
    if equiv x y then begin
      (* We have found a key [y] that is equivalent to [x]. *)
      s.population <- s.population - 1;
      (* Zap slot [j] and return [y]. *)
      zap s j y
    end
    else
      (* Skip this slot and continue searching. *)
      remove s x (next s j)

(* -------------------------------------------------------------------------- *)

(* Insertion: [add]. *)

(* We search for a key [x] and insert it if it is absent. *)

(* The Boolean result indicates whether [x] was inserted. *)

(* The fields [s.population] and [s.occupation] are updated. *)

(* If the table is a map, then the user supplies a value [v]
   in addition to the key [x], and this value is written to
   the [value] array. *)

let rec add (s : table) (x : key) ov (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    (* [x] is not in the table, and can be inserted here. *)
    K.unsafe_set s.key j x;
    #ifdef ENABLE_MAP
    cautiously_set_value s j v;
    #endif
    s.population <- s.population + 1;
    s.occupation <- s.occupation + 1;
    true
  end
  else if c == tomb then
    (* [x] might be in the table, somewhere beyond this tombstone.
       Search for it, and if we do not find it, then insert it
       here, at index [j]. *)
    let t = j in
    add_at_tombstone s x ov t (next s j)
  else
    let y = c in
    if equiv x y then
      (* We have found [x]. It is already present in the table. *)
      false
    else
      (* Skip this slot and continue searching. *)
      add s x ov (next s j)

(* [add_at_tombstone s x ov t j] searches for [x], starting from index [j].
   [t] must be the index of a tombstone.
   If [x] is not found then [x] is inserted at index [t],
   with value [ov],
   and [true] is returned.
   If [x] (or an equivalent key) is found then nothing happens
   and [false] is returned. *)

and add_at_tombstone (s : table) (x : key) ov (t : int) (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s t);
  assert (K.unsafe_get s.key t == tomb);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    (* [x] is not in the table. Insert it at index [t],
       which currently contains a tombstone. *)
    K.unsafe_set s.key t x;
    #ifdef ENABLE_MAP
    (* Because we have seen a tombstone, the [value] array must have
       been allocated already. *)
    set_value s t v;
    #endif
    s.population <- s.population + 1;
      (* [s.occupation] is unchanged. *)
    true
  end
  else if c == tomb then
    (* Skip this slot and continue searching. *)
    add_at_tombstone s x ov t (next s j)
  else
    let y = c in
    if equiv x y then begin
      (* A key [y] that is equivalent to [x] is already in the table. *)
      (* We could do nothing and return [false]. Instead, we write [y]
         at index [t], and zap slot [j]. This means that the next search
         for [x] or [y] will be faster. Furthermore, this can turn one or
         more occupied slots back into void slots. *)
      K.unsafe_set s.key t y;
      #ifdef ENABLE_MAP
      set_value s t (get_value s j);
      #endif
      (* Zap slot [j] and return [false]. *)
      zap s j false
    end
    else
      (* Skip this slot and continue searching. *)
      add_at_tombstone s x ov t (next s j)

(* In [add] (above), in case [c == tomb], one might be tempted to always
   overwrite the tombstone with [x], then call a variant of [remove] to find
   and remove any key [y] that is equivalent to [x] and that is already a
   member of the table. Unfortunately, this idea does not work. If the table
   already contains a key [y] that is equivalent to [x], then [add] is
   expected to leave [y] in the table; it must not replace [y] with [x]. *)

(* -------------------------------------------------------------------------- *)

(* Insertion: [add_absent]. *)

(* A special case of [add], where we assume that [x] is not in the table. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* The fields [s.population] and [s.occupation] are updated. *)

let rec add_absent (s : table) (x : key) ov (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    K.unsafe_set s.key j x;
    #ifdef ENABLE_MAP
    cautiously_set_value s j v;
    #endif
    s.population <- s.population + 1;
    s.occupation <- s.occupation + 1
  end
  else if c == tomb then begin
    (* Because [x] is not in the table, it can be safely inserted here,
       by overwriting this tombstone. *)
    K.unsafe_set s.key j x;
    #ifdef ENABLE_MAP
    set_value s j v;
    #endif
    s.population <- s.population + 1
    (* [s.occupation] is unchanged. *)
  end
  else
    let y = c in
    (* [x] is not in the table. *)
    assert (not (equiv x y));
    (* Skip this slot and continue searching. *)
    add_absent s x ov (next s j)

(* -------------------------------------------------------------------------- *)

(* Combined search and insertion: [find_key_else_add]. *)

(* [find_key_else_add] is analogous to [find_key], but inserts the key [x]
   into the table, if no key that is equivalent to [x] is found, before
   raising an exception. It is a combination of [find_key] and [add]. *)

let rec find_key_else_add (s : table) (x : key) ov (j : int) : key =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    (* [x] is not in the table. Insert it, then raise an exception. *)
    K.unsafe_set s.key j x;
    #ifdef ENABLE_MAP
    cautiously_set_value s j v;
    #endif
    s.population <- s.population + 1;
    s.occupation <- s.occupation + 1;
    raise Not_found
  end
  else if c == tomb then
    (* [x] might be in the table beyond this tombstone. *)
    let t = j in
    find_key_else_add_at_tombstone s x ov t j
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then y else find_key_else_add s x ov (next s j)

(* [find_key_else_add_at_tombstone s x ov t j] searches for [x], starting from [j].
   [t] must be the index of a tombstone.
   If [x] is not found then the key [x] inserted at index [t] with value [ov],
   and [Not_found] is raised.
   If [x] (or an equivalent key) is found then nothing happens. *)

and find_key_else_add_at_tombstone (s : table) (x : key) ov (t : int) (j : int) : key =
  assert (is_not_sentinel x);
  assert (is_index s t);
  assert (K.unsafe_get s.key t == tomb);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  if c == void then begin
    (* [x] is not in the table. Insert it at index [t],
       which currently contains a tombstone,
       then raise an exception. *)
    K.unsafe_set s.key t x;
    #ifdef ENABLE_MAP
    (* Because we have seen a tombstone, the [value] array must have
       been allocated already. *)
    set_value s t v;
    #endif
    s.population <- s.population + 1;
    (* [s.occupation] is unchanged. *)
    raise Not_found
  end
  else if c == tomb then
    (* Skip this slot and continue searching. *)
    find_key_else_add_at_tombstone s x ov t (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then begin
      (* A key [y] that is equivalent to [x] is already in the table. *)
      (* We could do nothing. Instead, we write [y] at index [t], and zap
         slot [j]. This means that the next search for [x] or [y] will be
         faster. Furthermore, this can turn one or more occupied slots back
         into void slots. *)
      K.unsafe_set s.key t y;
      #ifdef ENABLE_MAP
      set_value s t (get_value s j);
      #endif
      (* Zap slot [j] and return [y]. *)
      zap s j y
    end
    else
      (* Skip this slot and continue searching. *)
      find_key_else_add_at_tombstone s x ov t (next s j)

(* -------------------------------------------------------------------------- *)

(* [add_absent_no_updates] is a special case of [add], where:

   + we assume that [x] is not in the table;
   + we assume that there are no tombstones;
   + the fields [s.population] and [s.occupation] are NOT updated. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* This auxiliary function is used by [resize]. *)

let rec add_absent_no_updates (s : table) (x : key) ov (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = K.unsafe_get s.key j in
  assert (c != tomb);
  if c == void then begin
    K.unsafe_set s.key j x;
    #ifdef ENABLE_MAP
    cautiously_set_value s j v;
    #endif
  end
  else
    let y = c in
    assert (not (equiv x y));
    add_absent_no_updates s x ov (next s j)

(* -------------------------------------------------------------------------- *)

(* [resize s factor] allocates a new key array whose capacity is [factor]
   times the capacity of the current [key] array. Then, it copies the content
   of the old key array to the new one. *)

(* The [factor] parameter is typically 1 or 2. It must be a power of two. *)

(* The [value] array is resized in a similar way. *)

let resize (s : table) (factor : int) =
  assert (is_power_of_two factor);
  let old_key = s.key in
  #ifdef ENABLE_MAP
  let old_value = s.value in
  #endif
  let old_capacity = capacity s in
  let capacity = factor * old_capacity in
  s.mask <- capacity - 1;
  (* Resize the [key] array. *)
  s.key <- K.make capacity void;
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

let[@inline] length (s : table) (x : key) : int =
  (* No need to validate [x]; this function is private. *)
  length s x (start s x) 0

let[@inline] possibly_resize (s : table) =
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
  if crowded s || s.occupation = capacity s then
    (* Double the capacity of the [key] array. *)
    resize s 2;
  (* There must always remain at least one empty slot. Otherwise, searches
     would diverge. *)
  assert (s.occupation < capacity s)

let add (s : table) (x : key) ov : bool =
  validate x;
  let was_added = add s x ov (start s x) in
  if was_added then possibly_resize s;
  was_added

let add_absent (s : table) (x : key) ov =
  validate x;
  add_absent s x ov (start s x);
  possibly_resize s

let find_key_else_add (s : table) (x : key) ov =
  validate x;
  try
    find_key_else_add s x ov (start s x)
  with Not_found as e ->
    possibly_resize s;
    raise e

let[@inline] remove (s : table) (x : key) : key =
  validate x;
  remove s x (start s x)

let clear (s : table) =
  s.population <- 0;
  s.occupation <- 0;
  for k = 0 to capacity s - 1 do
    K.unsafe_set s.key k void
  done
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

let[@inline] cleanup (s : table) =
  (* If there are any tombstones, *)
  if s.occupation > s.population then
    (* then copy just the live keys to a new key array. *)
    resize s 1

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

let show_histogram (h : histogram) : string =
  let b = Buffer.create 128 in
  Printf.bprintf b "Average scan length: %.3f\n" (average h);
  Printf.bprintf b "Histogram:\n";
  IntMap.iter (fun l m ->
    Printf.bprintf b
      "  %9d keys require a linear scan of length %3d.\n"
      m l
  ) h;
  Buffer.contents b

let statistics (s : table) : string =
  Printf.sprintf "Population: %9d\nTombstones: %9d\nCapacity  : %9d\nOccupancy : %.3f\n"
    (population s) (s.occupation - population s) (capacity s) (occupancy s)
  ^ show_histogram (histogram s)

(* -------------------------------------------------------------------------- *)

(* Final packaging. *)

#ifdef ENABLE_MAP
type map = table
let iter = foreach_key_value
#else
type element = key
type set = table
let find = find_key
let find_else_add = find_key_else_add
let iter = foreach_key
#endif

end

(* -------------------------------------------------------------------------- *)

(* [MakeArray(X)] creates a copy of [Stdlib.Array] that is specialized for
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
#ifdef ENABLE_MAP
  let empty = [||]
#endif
  let make = Array.make
  let copy = Array.copy
  let length = Array.length
  let[@inline] unsafe_get (a : t) i = Array.unsafe_get a i
  let[@inline] unsafe_set (a : t) i x = Array.unsafe_set a i x
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
