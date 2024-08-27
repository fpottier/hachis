# To Do

## Features

* We should offer a variant of `remove` that does nothing
  when the key is absent, and returns `unit`.
  This variant should probably be called `remove`.
  Our current `remove` should be called `find_key_and_remove`.
  In maps, we may also wish to have `find_value_and_remove`.
  Adapt the benchmark accordingly.

* Publish `t` as a synonym for `set` or `map`.

* Implement `find_value_else_add`?

* Add a type `statistics` and a printer for it.

* Add a way of choosing the table's capacity and max occupancy rate
  at creation time. (Give `Make_` an extra parameter.) Check whether
  this degrades performance (with and without flambda).

* Add missing operations:
  `find` (synonym for `find_value` in hash maps),
  `find_opt`,
  `replace`,
  `filter_map_inplace`,
  `fold`,
  `length` (a synonym for `population`),
  `to_seq`, `add_seq`, `replace_seq`, `of_seq`.

* Develop a `Sentinel` module.
  `Make` extends an existing type with one fresh sentinel.
  `MakeMany` extends a type with `n` fresh sentinels.

## Algorithmic improvements

* In `cleanup`, to eliminate all tombstones, we currently copy the whole
  key array to a new key array of identical capacity. If there are very
  few tombstones, then this seems too costly: it requires re-hashing all
  keys. We could instead scan the key array, looking for tombstone runs.
  After each such run, the following keys must be potentially moved left.
  This amounts to removing and re-inserting these keys. (Beware of the
  circular array effect: at the beginning of the scan, until we find a
  tombstone or a void slot, we do not know whether the keys that we scan
  must be removed and re-inserted.)

## Test

* Add tests of `show`, with an expected output.

## Cleanup

* `clear` should use array `fill` instead of a loop.

* Finish cleaning up the code.

* Documentation: add a careful claim about speed. (Benchmark with and without flambda.)

* Release.
