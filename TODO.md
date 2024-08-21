# To Do

## Features

* We should offer a variant of `remove` that does nothing
  when the key is absent, and returns `unit`.
  This variant should probably be called `remove`.
  Our current `remove` should be called `find_key_and_remove`.
  In maps, we may also wish to have `find_value_and_remove`.
  Adapt the benchmark accordingly.

* Add a way of choosing a table's capacity and max occupancy rate,
  either by giving more parameters to the functor `Make_`,
  or by giving more parameters (with default values) to the function `create`.
  Check whether this degrades performance (with and without flambda).

* Add missing operations:
  `find_opt`,
  `replace`,
  `filter_map_inplace`,
  `fold`,
  `to_seq`, `add_seq`, `replace_seq`, `of_seq`.

* Check (improve) compatibility with `Stdlib.Hashtbl`,
  either by ensuring that our API matches the `Stdlib` API,
  or by providing a submodule that emulates the `Stdlib` API.
  E.g., need `length` as a synonym for `cardinal`.

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

* Finish cleaning up the code.

* Documentation: add a careful claim about speed. (Benchmark with and without flambda.)

* Declare a suitable version constraint on `cppo`.

* Release.
