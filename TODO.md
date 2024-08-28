# To Do

## Features

* We should offer a variant of `remove` that does nothing
  when the key is absent, and returns `unit`.
  This variant should probably be called `remove`.
  Our current `remove` should be called `find_key_and_remove`.
  In maps, we may also wish to have `find_value_and_remove`.
  Adapt the benchmark accordingly.

* Implement `find_value_else_add`?

* Add a type `statistics` and a printer for it.

* Add a way of choosing the table's capacity and max occupancy rate
  at creation time. (Give `Make_` an extra parameter.) Check whether
  this degrades performance (with and without flambda).

* Add missing operations:
  `find_opt`,
  `replace`,
  `filter_map_inplace`,
  `fold`,
  `length` (a synonym for `population`),
  `to_seq`, `add_seq`, `replace_seq`, `of_seq`.

* Check (improve) compatibility with `Stdlib.Hashtbl`,
  either by ensuring that our API matches the `Stdlib` API,
  or by providing a submodule that emulates the `Stdlib` API.

* Develop a `Sentinel` module.
  `Make` extends an existing type with one fresh sentinel.
  `MakeMany` extends a type with `n` fresh sentinels.

## Test

* Add tests of `show`, with an expected output.

## Cleanup

* `clear` should use array `fill` instead of a loop.

* Finish cleaning up the code.

* Documentation: add a careful claim about speed. (Benchmark with and without flambda.)

* Release.
