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

open Signatures

module[@inline] Make
(A : ARRAY)
(S : SENTINELS with type t = A.element)
(V : HashedType with type t = A.element)
= struct
open V
open S

type element =
  V.t

(* Although [equal] is traditionally named [equal], it is really
   an equivalence test. We rename it to [equiv] internally. *)

let equiv =
  equal

(* -------------------------------------------------------------------------- *)

(* In the main [data] array, the content of each slot can be:

   + [void],   an empty slot;
   + [tomb],   a slot that was once occupied, but is now empty; or
   + [x],      a slot that currently contains the element [x]. *)

(* The difference between [void] and [tomb] is that [void] stops a search,
   whereas [tomb] does not. In other words, when searching linearly for an
   element [x], if an empty slot is encountered, then the search stops, as
   the data structure's invariant guarantees that [x] cannot appear beyond
   this empty slot; whereas if a tombstone is encountered, then the search
   continues, as [x] could appear beyond this tombstone. In other words, we
   maintain the following invariant: if [x] is in the set then it must
   appear between the index [start s x] and the first [void] slot. *)

(* Furthermore (this is optional), we maintain the invariant that a [tomb]
   slot is never followed with a [void] slot. To achieve this, in [remove],
   if the element that is being removed is followed with [void], then this
   element and all preceding tombstones are overwritten with [void]. This
   makes [remove] more costly but allows us to maintain a lower occupancy. *)

let forbid_tomb_void =
  true

(* Instead of using an algebraic data type, as follows:

     type content =
       | Void
       | Tomb
       | Data of element

   we represent [void] and [tomb] as two sentinel values, that is, two
   special values that the user is not allowed to insert into the set.
   This allows us to allocate fewer memory blocks and to use just an
   array of elements.

   We assume that a sentinel can be recognized using [==]. *)

type content =
  element

let[@inline] is_sentinel (c : content) =
  c == void || c == tomb

let[@inline] is_not_sentinel (c : content) =
  not (is_sentinel c)

(* A hash set is represented as follows. *)

type set = {
  (* The number of data elements in the [data] array. *)
  mutable population : int;
  (* The number of data and [tomb] elements in the [data] array. *)
  mutable occupied   : int;
  (* The capacity of the [data] array, minus one. *)
  mutable mask       : int;
  (* The data array. The length of this array is a power of two. *)
  mutable data       : A.t;
}

(* A hash code is an arbitrary integer. *)

type hash =
  int

(* An index into the [data] array. *)

type index =
  int

(* A population count. *)

type population =
  int

(* An array size. *)

type capacity =
  int

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

(* The definition of occupancy is based on [s.occupied], which counts both
   empty slots and tombstones. This is required to ensure that every linear
   search terminates.

   Indeed, imagine what could happen if occupancy counted empty slots only.
   Imagine that the [data] array is filled with tombstones. Then, occupancy
   would be zero, yet every search would diverge, as it would never find an
   empty slot. *)

let[@inline] population (s : set) : population =
  s.population

let[@inline] capacity (s : set) : capacity =
  A.length s.data

let[@inline] occupancy (s : set) : float =
  float (s.occupied) /. float (capacity s)

(* -------------------------------------------------------------------------- *)

(* [index s h] converts the hash code [h] to an index into the [data] array. *)

let[@inline] index (s : set) (h : hash) : index =
  (* Because the length of the [data] array is a power of two,
     the desired index can be computed by keeping just the least
     significant bits of the hash code [h], as follows. *)
  h land s.mask

(* [start s x] is the index where a search for [x] begins. *)

let[@inline] start (s : set) (x : element) : index =
  index s (hash x)

(* [next s i] increments the index [i] into the [data] array, while handling
   wrap-around. *)

let[@inline] next (s : set) (i : index) : index =
  (i + 1) land s.mask

(* [prev s i] decrements the index [i] into the [data] array, while handling
   wrap-around. *)

let[@inline] prev (s : set) (i : index) : index =
  (i - 1) land s.mask

(* [is_index s i] checks that [i] is valid index into the [data ]array. *)

let[@inline] is_index (s : set) (i : index) : bool =
  0 <= i && i < capacity s

(* -------------------------------------------------------------------------- *)

(* The functions [is_power_of_two] and [check] are used only during testing.  *)

let rec is_power_of_two c =
  c = 1 || is_power_of_two (c / 2)

let check s =
  assert begin
    let capacity = capacity s in
    assert (0 < capacity);
    assert (is_power_of_two capacity);
    assert (s.mask = capacity - 1);
    assert (0 <= s.population && s.population <= capacity);
    assert (0 <= s.occupied && s.occupied <= capacity);
    let pop, occ = ref 0, ref 0 in
    for k = 0 to capacity - 1 do
      let content = A.unsafe_get s.data k in
      if content == void then
        ()
      else if content == tomb then begin
        incr occ;
        if forbid_tomb_void then
          (* [tomb] is never followed with [void]. *)
          assert (A.unsafe_get s.data (next s k) != void)
      end
      else begin
        incr occ; incr pop
      end
    done;
    assert (s.population = !pop);
    assert (s.occupied = !occ);
    true
  end

(* -------------------------------------------------------------------------- *)

(* Two parameters: initial capacity and maximal occupancy. *)

(* To ensure that the linear search terminates, one must guarantee that there
   is always at least one [void] slot in the [data] array. This property must
   hold also after an insertion, as the question "should we resize?" is asked
   after each insertion (as opposed to before each insertion).

   Thus, we must guarantee that, before an insertion, there exist at least two
   [void] slots. To guarantee this, it is sufficient to initially enforce
   [max_occupancy + 2/initial_capacity <= 1].
   Thereafter, the capacity of the [data] array can only grow, so
   [max_occupancy + 2/capacity <= 1]
   must be true as well. *)

let initial_capacity =
  16

(* To avoid floating-point computations, we express [max_occupancy] as an
   integer value, in percent. *)

let max_occupancy_percent =
  82

let max_occupancy =
  float max_occupancy_percent /. 100.0

let () =
  if not (max_occupancy +. 2.0 /. float initial_capacity <= 1.0) then
    assert false
    (* This assertion is kept in [release] mode. *)

let[@inline] crowded s =
  (* The test is performed using integer arithmetic, *)
  let result = 100 * s.occupied > max_occupancy_percent * capacity s in
  (* but is equivalent to a test expressed in floating-point arithmetic: *)
  assert (result = (occupancy s > max_occupancy));
  result

(* -------------------------------------------------------------------------- *)

(* Membership tests: [mem] and [find]. *)

(* We search for an element [x] in order to determine whether [x]
   (or some element that is equivalent to [x]) is present in the set. *)

(* [j] is the index that is currently under examination. *)

(* The Boolean result indicates whether [x] was found. *)

let rec mem (s : set) (x : element) (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then
    (* [x] is not in the set. *)
    false
  else if c == tomb then
    (* [x] might be in the set beyond this tombstone. *)
    mem s x (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have succeeded;
       otherwise, skip this slot and continue searching. *)
    equiv x y || mem s x (next s j)

(* [find] is analogous to [mem], but returns the element [y] that is found,
   and raises an exception if no element that is equivalent to [x] is found. *)

let rec find (s : set) (x : element) (j : int) : element =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then
    (* [x] is not in the set. *)
    raise Not_found
  else if c == tomb then
    (* [x] might be in the set beyond this tombstone. *)
    find s x (next s j)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then y else find s x (next s j)

(* [length] is analogous to [mem], but measures the length of the linear
   scan that is required to find [x]. It is used by [statistics]. *)

let rec length (s : set) (x : element) (j : int) (accu : int) : int =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then
    (* [x] is not in the set. *)
    accu
  else if c == tomb then
    (* [x] might be in the set beyond this tombstone. *)
    length s x (next s j) (accu + 1)
  else
    let y = c in
    (* If [x] and [y] are equivalent, then we have succeeded;
       otherwise, skip this slot and continue searching. *)
    if equiv x y then accu else length s x (next s j) (accu + 1)

(* -------------------------------------------------------------------------- *)

(* [zap s j v] zaps slot [j] (which must contain an element) and returns [v]. *)

(* To zap a slot means to overwrite this slot with [tomb] or [void]. *)

(* Overwriting a slot with [void] is correct only if the next slot is
   [void] already. *)

(* [s.population] is not affected. *)

(* [s.occupied] is decreased by the number of [void] slots that we create. *)

let zap s j v =
  assert (is_index s j);
  assert (is_not_sentinel (A.unsafe_get s.data j));
  (* Test whether the next slot is void. *)
  if forbid_tomb_void && A.unsafe_get s.data (next s j) == void then begin
    (* The next slot is void. In order to maintain the invariant
       that [tomb] is never followed with [void], we must replace
       [x], as well as all previous tombstones, with [void]. *)
    A.unsafe_set s.data j void;
    let k = ref (prev s j) in
    let count = ref 1 in
    while A.unsafe_get s.data !k == tomb do
      A.unsafe_set s.data !k void;
      k := prev s !k;
      count := !count + 1
    done;
    (* [s.occupied] is decreased by the number of [void] slots
       that we have been able to recreate. *)
    s.occupied <- s.occupied - !count
  end
  else begin
    (* The next slot is not void, or we do not forbid [tomb] followed
       with [void]. Write a tombstone at index [j]. *)
    A.unsafe_set s.data j tomb
    (* [s.occupied] is unchanged. *)
  end;
  v

(* -------------------------------------------------------------------------- *)

(* Deletion: [remove]. *)

(* We search for an element [x] and remove it if it is present. *)

(* The Boolean result indicates whether [x] was found and removed. *)

(* The fields [s.population] and [s.occupied] are updated. *)

let rec remove (s : set) (x : element) (j : int) : element =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then
    (* [x] is not in the set. *)
    raise Not_found
  else if c == tomb then
    (* [x] might be in the set beyond this tombstone. *)
    remove s x (next s j)
  else
    let y = c in
    if equiv x y then begin
      (* We have found an element [y] that is equivalent to [x]. *)
      s.population <- s.population - 1;
      (* Zap slot [j] and return [y]. *)
      zap s j y
    end
    else
      (* Skip this slot and continue searching. *)
      remove s x (next s j)

(* -------------------------------------------------------------------------- *)

(* Insertion: [add]. *)

(* We search for an element [x] and insert it if it is absent. *)

(* The Boolean result indicates whether [x] was inserted. *)

(* The fields [s.population] and [s.occupied] are updated. *)

let rec add (s : set) (x : element) (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then begin
    (* [x] is not in the set, and can be inserted here. *)
    A.unsafe_set s.data j x;
    s.population <- s.population + 1;
    s.occupied <- s.occupied + 1;
    true
  end
  else if c == tomb then
    (* [x] might be in the set, somewhere beyond this tombstone.
       Search for it, and if we do not find it, then insert it
       here, at index [j]. *)
    let t = j in
    add_at_tombstone s x t (next s j)
  else begin
    let y = c in
    if equiv x y then
      (* We have found [x]. It is already present in the set. *)
      false
    else
      (* Skip this slot and continue searching. *)
      add s x (next s j)
  end

(* [add_at_tombstone s x t j] searches for [x], starting from index [j].
   [t] must be the index of a tombstone.
   If [x] is not found then [x] is inserted at index [t],
   and [true] is returned.
   If [x] (or an equivalent element) is found then nothing happens
   and [false] is returned. *)

and add_at_tombstone (s : set) (x : element) (t : int) (j : int) : bool =
  assert (is_not_sentinel x);
  assert (is_index s t);
  assert (A.unsafe_get s.data t == tomb);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then begin
    (* [x] is not in the set. Insert it at index [t],
       which currently contains a tombstone. *)
    A.unsafe_set s.data t x;
    s.population <- s.population + 1;
      (* [s.occupied] is unchanged. *)
    true
  end
  else if c == tomb then
    (* Skip this slot and continue searching. *)
    add_at_tombstone s x t (next s j)
  else begin
    let y = c in
    if equiv x y then begin
      (* An element [y] that is equivalent to [x] is already in the set. *)
      (* We could do nothing and return [false]. Instead, we write [y]
         at index [t], and zap slot [j]. This means that the next search
         for [x] or [y] will be faster. Furthermore, this can turn one or
         more occupied slots back into void slots. *)
      A.unsafe_set s.data t y;
      (* Zap slot [j] and return [false]. *)
      zap s j false
    end
    else
      (* Skip this slot and continue searching. *)
      add_at_tombstone s x t (next s j)
  end

(* In [add] (above), in case [c == tomb], one might be tempted to always
   overwrite the tombstone with [x], then call a variant of [remove] to find
   and remove any element [y] that is equivalent to [x] and that is already a
   member of the set. Unfortunately, this idea does not work. If the set
   already contains an element [y] that is equivalent to [x], then [add] is
   expected to leave [y] in the set; it must not replace [y] with [x]. *)

(* -------------------------------------------------------------------------- *)

(* Insertion: [add_absent]. *)

(* A special case of [add], where we assume that [x] is not in the set. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* The fields [s.population] and [s.occupied] are updated. *)

let rec add_absent (s : set) (x : element) (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then begin
    A.unsafe_set s.data j x;
    s.population <- s.population + 1;
    s.occupied <- s.occupied + 1
  end
  else if c == tomb then begin
    (* Because [x] is not in the set, it can be safely inserted here,
       by overwriting this tombstone. *)
    A.unsafe_set s.data j x;
    s.population <- s.population + 1
    (* [s.occupied] is unchanged. *)
  end
  else begin
    let y = c in
    (* [x] is not in the set. *)
    assert (not (equiv x y));
    (* Skip this slot and continue searching. *)
    add_absent s x (next s j)
  end

(* -------------------------------------------------------------------------- *)

(* Combined search and insertion: [find_else_add]. *)

(* [find_else_add] is analogous to [find], but inserts the element [x] into
   the set, if no element that is equivalent to [x] is found, before raising
   an exception. It is a combination of [find] and [add]. *)

let rec find_else_add (s : set) (x : element) (j : int) : element =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then begin
    (* [x] is not in the set. Insert it, then raise an exception. *)
    A.unsafe_set s.data j x;
    s.population <- s.population + 1;
    s.occupied <- s.occupied + 1;
    raise Not_found
  end
  else if c == tomb then
    (* [x] might be in the set beyond this tombstone. *)
    let t = j in
    find_else_add_at_tombstone s x t j
  else begin
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then y else find_else_add s x (next s j)
  end

(* [find_else_add_at_tombstone s x t j] searches for [x], starting from [j].
   [t] must be the index of a tombstone.
   If [x] is not found then [x] is inserted at index [t],
   and [Not_found] is raised.
   If [x] (or an equivalent element) is found then nothing happens. *)

and find_else_add_at_tombstone (s : set) (x : element) (t : int) (j : int) : element =
  assert (is_not_sentinel x);
  assert (is_index s t);
  assert (A.unsafe_get s.data t == tomb);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  if c == void then begin
    (* [x] is not in the set. Insert it at index [t],
       which currently contains a tombstone,
       then raise an exception. *)
    A.unsafe_set s.data t x;
    s.population <- s.population + 1;
    (* [s.occupied] is unchanged. *)
    raise Not_found
  end
  else if c == tomb then
    (* Skip this slot and continue searching. *)
    find_else_add_at_tombstone s x t (next s j)
  else begin
    let y = c in
    (* If [x] and [y] are equivalent, then we have found [y];
       otherwise, skip this slot and continue searching. *)
    if equiv x y then begin
      (* An element [y] that is equivalent to [x] is already in the set. *)
      (* We could do nothing. Instead, we write [y] at index [t], and zap
         slot [j]. This means that the next search for [x] or [y] will be
         faster. Furthermore, this can turn one or more occupied slots back
         into void slots. *)
      A.unsafe_set s.data t y;
      (* Zap slot [j] and return [y]. *)
      zap s j y
    end
    else
      (* Skip this slot and continue searching. *)
      find_else_add_at_tombstone s x t (next s j)
  end

(* -------------------------------------------------------------------------- *)

(* [add_absent_no_updates] is a special case of [add], where:

   + we assume that [x] is not in the set;
   + we assume that there are no tombstones;
   + the fields [s.population] and [s.occupied] are NOT updated. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* This auxiliary function is used by [resize]. *)

let rec add_absent_no_updates (s : set) (x : element) (j : int) =
  assert (is_not_sentinel x);
  assert (is_index s j);
  let c = A.unsafe_get s.data j in
  assert (c != tomb);
  if c == void then
    A.unsafe_set s.data j x
  else
    let y = c in
    assert (not (equiv x y));
    add_absent_no_updates s x (next s j)

(* -------------------------------------------------------------------------- *)

(* [resize s factor] allocates a new data array whose capacity is [factor]
   times the capacity of the current [data] array. Then, it copies the content
   of the old data array to the new one. *)

(* The [factor] parameter is typically 1 or 2. It must be a power of two. *)

let resize (s : set) (factor : int) =
  assert (is_power_of_two factor);
  let old_data = s.data in
  let old_capacity = capacity s in
  let capacity = factor * old_capacity in
  s.mask <- capacity - 1;
  s.data <- A.make capacity void;
  (* At this point, [s] is a valid empty set, except for the [population]
     and [occupied] fields. *)
  (* Every element of the old data array must now be inserted into [s]. Each
     insertion operation inserts a new element (one that is not already
     present), and no tombstones can be encountered. Also, the [population]
     and [occupied] fields need not be updated. Thus, [add_absent_no_updates]
     is used. *)
  for k = 0 to old_capacity - 1 do
    let c = A.unsafe_get old_data k in
    if is_not_sentinel c then
      let x = c in
      add_absent_no_updates s x (start s x)
  done;
  (* The population is unchanged. There are no tombstones any more,
     so [s.occupied] now coincides with [s.population]. *)
  s.occupied <- s.population

(* -------------------------------------------------------------------------- *)

(* Public functions. *)

let create () =
  let capacity = initial_capacity in
  let population = 0
  and occupied = 0
  and mask = capacity - 1
  and data = A.make capacity void in
  { population; occupied; mask; data }

let[@inline] validate (x : element) =
  assert (is_not_sentinel x)
    (* We use an assertion that is erased in release mode.
       If we wanted this module to be more defensive, we
       could keep a defensive test in release mode. *)

let[@inline] mem (s : set) (x : element) : bool =
  validate x;
  mem s x (start s x)

let[@inline] find (s : set) (x : element) : element =
  validate x;
  find s x (start s x)

let[@inline] length (s : set) (x : element) : int =
  validate x;
  length s x (start s x) 0

let[@inline] possibly_resize (s : set) =
  (* If the maximum occupancy is now exceeded, then the capacity of the [data]
     array must be increased. (It is doubled.) *)
  if crowded s then
    resize s 2;
  (* There must always remain at least one empty slot. Otherwise, searches
     would diverge. *)
  assert (population s < capacity s)

let add (s : set) (x : element) : bool =
  validate x;
  let was_added = add s x (start s x) in
  if was_added then possibly_resize s;
  was_added

let add_absent (s : set) (x : element) =
  validate x;
  add_absent s x (start s x);
  possibly_resize s

let find_else_add (s : set) (x : element) =
  validate x;
  try
    find_else_add s x (start s x)
  with Not_found as e ->
    possibly_resize s;
    raise e

let[@inline] remove (s : set) (x : element) : element =
  validate x;
  remove s x (start s x)

let clear (s : set) =
  s.population <- 0;
  s.occupied <- 0;
  for k = 0 to capacity s - 1 do
    A.unsafe_set s.data k void
  done

let reset (s : set) =
  let capacity = initial_capacity in
  let population = 0
  and occupied = 0
  and mask = capacity - 1
  and data = A.make capacity void in
  s.population <- population;
  s.occupied <- occupied;
  s.mask <- mask;
  s.data <- data

let[@inline] cleanup (s : set) =
  (* If there are any tombstones, *)
  if s.occupied > s.population then
    (* then copy just the live keys to a new data array. *)
    resize s 1

(* [array_copy] copies a [data] array. We could in principle use a [fill]
   function, but the module [A] does not offer one, so we use a loop. *)

let array_copy (old_data : A.t) : A.t =
  let capacity = A.length old_data in
  let new_data = A.make capacity void in
  for k = 0 to capacity - 1 do
    let c = A.unsafe_get old_data k in
    if c != void then
      A.unsafe_set new_data k c
  done;
  new_data

(* One might ask whether [copy] should return an identical copy or
   construct a fresh hash set that does not contain any tombstones. We
   choose the first option, because it is simpler and more efficient;
   it does not require hashing. *)

let copy (s : set) : set =
  { s with data = array_copy s.data }

let iter f (s : set) =
  for i = 0 to A.length s.data - 1 do
    let c = A.unsafe_get s.data i in
    if is_not_sentinel c then
      let x = c in
      f x
  done

let separated iter show sep v =
  let b = Buffer.create 32 in
  let first = ref true in
  iter (fun x ->
    if not !first then Buffer.add_string b sep;
    Buffer.add_string b (show x);
    first := false
  ) v;
  Buffer.contents b

let show show (s : set) =
  "{" ^
  separated iter show ", " s ^
  "}"

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

let histogram (s : set) : histogram =
  let h = ref IntMap.empty in
  iter (fun x ->
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
      "  %9d elements require a linear scan of length %3d.\n"
      m l
  ) h;
  Buffer.contents b

let statistics (s : set) : string =
  Printf.sprintf "Population: %9d\nTombstones: %9d\nCapacity  : %9d\nOccupancy : %.3f\n"
    (population s) (s.occupied - population s) (capacity s) (occupancy s)
  ^ show_histogram (histogram s)

end
