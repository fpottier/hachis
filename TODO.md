# To Do

* Add hash maps, while sharing code, if possible.
  In `HashMap`, add `foreach_key`, which reads just the keys.

* Benchmark more operations.

* Require the module `A` to offer `copy`?

* Add a way of choosing the table's capacity and max occupancy rate
  at creation time.

  Add a way of decreasing an existing table's capacity so as
  to fit its population.

* Add a type `statistics` and a printer for it.

* Add missing operations:
  `find_opt`,
  `replace`,
  `filter_map_inplace`,
  `fold`,
  `length` (a synonym for `population`),
  `to_seq`, `add_seq`, `replace_seq`, `of_seq`.

* Write documentation. Note that this data structure is sequential.
  Note that it does not protect against memory leaks.

* Develop a hash map variant? (Using an additional `value` array.)

* Develop a `Sentinel` module.
  `Make` extends an existing type with one fresh sentinel.
  `MakeMany` extends an extends type with `n` fresh sentinels.

* Release.
