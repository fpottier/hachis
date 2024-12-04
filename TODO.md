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
  See [this post](https://ibraheem.ca/posts/designing-papaya/).
  And [this one](https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/).
  Or [this talk](https://www.youtube.com/watch?v=HJ-719EGIts) by Cliff Click.
  And [this paper](https://dl.acm.org/doi/10.1145/3309206).
  And [this one](https://dl.acm.org/doi/abs/10.1145/3016078.2851196).
  And [Hopscotch hashing](https://people.csail.mit.edu/shanir/publications/disc2008_submission_98.pdf).
  If we do not need an exact computation of the load factor
  then an approximate population count could suffice
  (but it is still desirable to have a constant-time exact
   population count, when requested by the user).

  (A different, lock-based approach would be to divide the arrays into pages
   and to use one readers-writer lock per page. To avoid deadlocks,
   each thread must hold at most one lock at a time.)
  (Another lock-based approach is to use just one lock for the whole table,
   and to use sharding, that is, to implement a single logical table
   using a collection of tables, using the hash function to decide
   which keys are distributed into which tables. See `dashmap` in Rust.)
