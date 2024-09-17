(* -------------------------------------------------------------------------- *)

(* Insertion: [add_absent]. *)

(* A special case of [add_if_absent], where:
   + we assume that [x] is not in the table. *)

(* [x] is always inserted. No Boolean result is returned. *)

(* The fields [s.population] and [s.occupation] are updated. *)

(* Some benchmarks suggest that:
   - if there are no tombstones then [add_absent] has the same performance
     as [replace] and [add_if_absent];
   - if there are some tombstones then [add_absent] can be marginally
     faster than [replace] and [add_if_absent].

   Because [add_absent] has a nontrivial precondition (that is, [x] must
   be absent), it is an unsafe function. If [x] is in fact present in the
   set [s], then calling [add_absent s x] will violate the invariant; two
   copies of [x] will exist in the set. For this reason, we prefer to not
   make [add_absent] available to the end user. *)

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

(* The public function. *)

let add_absent (s : table) (x : key) ov =
  validate x;
  add_absent s x ov (start s x);
  possibly_grow s
