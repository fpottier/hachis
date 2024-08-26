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

* Add `fit`, which decreases an existing table's capacity so as
  to fit its population.

* Add a type `statistics` and a printer for it.

* Add a way of choosing the table's capacity and max occupancy rate
  at creation time.

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

## Test

* Add tests of `show`, with an expected output.

## Benchmark

* Benchmark more operations, including `mem`.

## Cleanup

* Finish cleaning up the code.

* Write documentation. Note that this data structure is sequential.
  Note that it does not protect against memory leaks.
  Note that iteration can be slow unless `fit` has been called.

* Release.
