# To Do

## Features

* Implement `find_value_else_add`?

* Add `fit`, which decreases an existing table's capacity so as
  to fit its population.

* Add a type `statistics` and a printer for it.

* Add a way of choosing the table's capacity and max occupancy rate
  at creation time.

* Add missing operations:
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

* Find out why inserting consecutive integers into a balanced binary tree
  is 10x faster than inserting random integers.

## Cleanup

* Finish cleaning up the code.

* Write documentation. Note that this data structure is sequential.
  Note that it does not protect against memory leaks.
  Note that iteration can be slow unless `fit` has been called.

* Release.
