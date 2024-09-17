# To Do

## Features

* Add a way of choosing a table's initial capacity and max occupancy rate,
  either by giving more parameters to the functor `Make_`,
  or by giving more parameters (with default values) to the function `create`.
  Check whether this degrades performance (with and without flambda).

* Add missing operations:
  `filter_map_inplace`,
  `fold`,
  `to_seq`, `add_seq`, `replace_seq`, `of_seq`,
  `to_seq_keys`, `to_seq_values`.

* Check (improve) compatibility with `Stdlib.Hashtbl`,
  either by ensuring that our API matches the `Stdlib` API,
  or by providing a submodule that emulates the `Stdlib` API.
  + `create` : take capacity as an argument
  + `add` and `replace` : return nothing
  + need `find_opt`
  + need `length` as a synonym for `cardinal`

* Develop a `Sentinel` module.
  `Make` extends an existing type with one fresh sentinel.
  `MakeMany` extends a type with `n` fresh sentinels.

* Think about a concurrent variant of this data structure.
  Shrinking or growing the table requires blocking
  insertions and deletions (but not lookups).
  An insertion or deletion operation can be committed by
  a CAS on the `key` array. The table's `population` and
  `occupation` fields can be updated (via fetch-and-add)
  after the commit point, but this seems to imply that
  the current value of these fields can never be trusted
  unless the lock has been taken.

## Cleanup

* Benchmark: understand why "a bit of everything" does not give us a large
  edge.

* Benchmark using `pbench`. Keep the number of operations just high enough to
  obtain meaningful measurements. Test with a range of table sizes.

* Documentation: add a careful claim about speed.
  (Benchmark with OCaml 4 and 5, with and without flambda.)

* Release.
