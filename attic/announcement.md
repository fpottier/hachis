It is my pleasure to announce the first release of `hachis`, a library that offers hash sets and hash maps.

These data structures handle collisions via linear probing, a technique that relies on linear searches within an array. All of the data is stored within one large array (for hash sets) or two large arrays (for hash maps). As a result, these data structures offer good locality.

Some benchmarks suggest that `hachis` can consistently outperform the standard library's hash maps (`Hashtbl`).

To install the library, type `opam update && opam install hachis`.

For more details, see the [documentation](https://cambium.inria.fr/~fpottier/hachis/doc/hachis/).
