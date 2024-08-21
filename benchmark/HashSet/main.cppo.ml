(******************************************************************************)
(*                                                                            *)
(*                                   Hachis                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(******************************************************************************)

open Printf

module B = Common.Benchmark
let run benchmarks =
  List.iter B.drive_and_display benchmarks

let quota =
  "5.0s"

let () =
  Random.init 42

(* -------------------------------------------------------------------------- *)

(* This benchmark compares several implementations of a minimal HashSet API.  *)

(* In [Stdlib.Hashtbl], [add] and [remove] return a result of type [unit],
   as opposed to [bool], so we have to adopt the same impoverished API. *)

(* In this API, the semantics of [add] is to replace an existing entry if
   there is one. The semantics of [remove] is to do nothing if there is no
   existing entry. *)

module type API = sig
  type element = int
  type set
  val create : unit -> set
  val add : set -> element -> unit
  val add_absent : set -> element -> unit
  val remove : set -> element -> unit
  val mem : set -> element -> bool
end

(* -------------------------------------------------------------------------- *)

(* The implementations that we wish to benchmark. *)

(* We use nonnegative integer values, and equip them with an equivalence
   relation that is not equality. We view two integer values as equivalent
   if they are equal except possibly in their least significant bit. *)
module V = struct
  type t = int
  let normalize x = x lor 1
  let equal x y = Int.equal (normalize x) (normalize y)
  let hash x = Hashtbl.hash (normalize x)
    (* In OCaml 5.1, [Int.hash] could be preferred. *)
  let compare x y = Int.compare (normalize x) (normalize y)
end

module A = struct
  include Array
  type element = int
  type t = element array
  let empty = [||]
end

(* We use -1 and -2 as sentinels. Our generators must be careful not
   to produce these values. *)
module S = struct type t = int let void = (-1) let tomb = (-2) end

(* Instantiate Hachis.HashSet. *)

module HashSet : API = struct
  include Hachis.HashSet.Make(A)(S)(V)
  let[@inline] add s x = ignore (add s x)
  let[@inline] remove s x =
    try ignore (remove s x) with Not_found -> ()
end

(* Instantiate [Hachis.HashSet] using [Hector.IntArray]. *)

module HectorHashSet : API = struct
  include Hachis.HashSet.Make(Hector.IntArray)(S)(V)
  let[@inline] add s x = ignore (add s x)
  let[@inline] remove s x =
    try ignore (remove s x) with Not_found -> ()
end

(* Instantiate [Hachis.HashMap] so as to respect [API]. *)

module HashMap : API = struct
  include Hachis.HashMap.Make(A)(S)(V)(A)
  type element = key
  type set = map
  let[@inline] add s x = ignore (add s x x)
  let[@inline] add_absent s x = add_absent s x x
  let[@inline] remove s x =
    try ignore (remove s x) with Not_found -> ()
end

(* Instantiate [Stdlib.Hashtbl] so as to respect [API]. *)

(* In [Stdlib.Hashtbl], [add] hides an existing entry, if there is
   one, so a key can be present several times in the table. We must
   avoid this: we must implement [add] using [replace]. *)

module Hashtbl : API = struct
  include Stdlib.Hashtbl.Make(V)
  type element = int
  type set = unit t
  let[@inline] create () = create 128
  let[@inline] add s x = replace s x ()
  and[@inline] add_absent s x = add s x ()
end

(* Instantiate [Stdlib.Set] so as to respect [API]. *)

module Set : API = struct
  open Set.Make(V)
  type element = int
  type set = t ref
  let[@inline] create () = ref empty
  let[@inline] add s x = (s := add x !s)
  let add_absent = add
  let[@inline] remove s x = (s := remove x !s)
  let[@inline] mem s x = mem x !s
end

(* Instantiate [Baby.W.Set] so as to respect [API]. *)

module BabyWSet : API = struct
  open Baby.W.Set.Make(V)
  type element = int
  type set = t ref
  let[@inline] create () = ref empty
  let[@inline] add s x = (s := add x !s)
  let add_absent = add
  let[@inline] remove s x = (s := remove x !s)
  let[@inline] mem s x = mem x !s
end

(* We use the module [R] in the generation of benchmark scenarios. *)

(* It enriches the underlying set with a maximum population field.
   [reset_max_pop] sets the maximum population field to the current
   population. [get_max_pop] reads the maximum population field. *)

(* Furthermore, [is_empty] determines whether the set is empty, and
   [choose] picks a random element out of a nonempty set. *)

module R = struct
  open Baby.W.Set.Make(V)
  type t = { mutable max_pop: int; mutable now: set }
  let create () = { max_pop = 0; now = empty }
  let add s x =
    s.now <- add x s.now;
    let n = cardinal s.now in
    if s.max_pop < n then s.max_pop <- n
  let remove s x = (s.now <- remove x s.now)
  let mem s x = mem x s.now
  let is_empty s = is_empty s.now
  let choose s =
    let n = cardinal s.now in
    assert (n > 0);
    let i = Random.int n in
    get s.now i
  let reset_max_pop s = s.max_pop <- cardinal s.now
  let get_max_pop s = s.max_pop
end

(* -------------------------------------------------------------------------- *)

(* Each benchmark is defined as a macro (not a higher-order function)
   because we want to benchmark realistic client code, where calls to
   library functions are inlined (when possible). *)

(* -------------------------------------------------------------------------- *)

(* Scenario generation. *)

type key =
  V.t

(* An instruction type is one of the following. *)

type argument =
  | Key of key (* a specific key *)
  | Random     (* any random key *)
  | Absent     (* any currently absent key *)
  | Present    (* any currently present key *)

type itype =
  | ITAdd of argument
  | ITAddAbsent
  | ITRemove of argument
  | ITMem of argument

(* A concrete instruction is [add x] or [remove x]. *)

type instruction =
  | IAdd of key
  | IAddAbsent of key
  | IRemove of key
  | IMem of key

(* A scenario consists of two sequences of instructions.
   The first sequence is not timed, and can be used to
   prepare the hash set. The second sequence is timed. *)

type sequence =
  instruction array

type scenario =
  sequence * sequence

(* A recipe is a pair of an integer length [n] and a
   function of an index [i] to an instruction type. *)

type recipe =
  int * (int -> itype)

let empty : recipe =
  0, (fun _ -> assert false)

(* A scenario can be created out of a pair of recipes. *)

type recipes =
  recipe * recipe

(* [choose_key a s u] randomly chooses an integer value inside or
   outside of the set [s], depending on [a]. If it must be chosen
   outside of [s], then it is randomly drawn below [u]. *)

let rec choose_key (a : argument) s u : key =
  match a with
  | Key x ->
      x
  | Random ->
      Random.int u
  | Absent ->
      let x = Random.int u in
      if R.mem s x then
        choose_key a s u (* retry *)
      else
        x
  | Present ->
      if R.is_empty s then
        (* The request cannot be honored. Never mind. *)
        choose_key Absent s u
      else
        R.choose s

(* [choose_instruction s u it] randomly chooses a concrete instruction
   that corresponds to the instruction type [it]. This choice depends
   on the current state [s]. Furthermore, as a result of this choice,
   [s] is updated. *)

let choose_instruction s u (it : itype) : instruction =
  match it with
  | ITAdd a ->
      let x = choose_key a s u in
      R.add s x;
      IAdd x
  | ITAddAbsent ->
      let a = Absent in
      let x = choose_key a s u in
      R.add s x;
      IAddAbsent x
  | ITRemove a ->
      let x = choose_key a s u in
      R.remove s x;
      IRemove x
  | ITMem a ->
      let x = choose_key a s u in
      IMem x

(* [choose_sequence s u r] randomly chooses an instruction sequence
   that obeys the recipe [r]. *)

let choose_sequence s u (r : recipe) : sequence =
  let n, (it : int -> itype) = r in
  Array.init n @@ fun i ->
    choose_instruction s u (it i)

(* [print_statistics] prints statistics about a sequence of instructions. *)

let print_statistics (seq : sequence) =
  let add, remove, mem = ref 0, ref 0, ref 0 in
  Array.iter (fun instruction ->
    match instruction with
    | IAdd       _ -> incr add
    | IAddAbsent _ -> incr add
    | IRemove    _ -> incr remove
    | IMem       _ -> incr mem
  ) seq;
  printf "This scenario involves %d insertions, %d deletions, and %d lookups.\n"
    !add !remove !mem

(* [choose_scenario u (r1, r2)] randomly chooses a scenario that begins with
   an empty set as the initial state and obeys the pair of recipes [(r1, r2)].
   The recipe [r1] is used to generate a first sequence of instructions
   (initialization). The recipe [r2] is used to generate a second sequence of
   instructions (use). *)

let choose_scenario u (r1, r2 : recipes) : scenario =
  let s = R.create() in
  let seq1 = choose_sequence s u r1 in
  R.reset_max_pop s;
  let initial_pop = R.get_max_pop s in
  let seq2 = choose_sequence s u r2 in
  let max_pop = R.get_max_pop s in
  print_statistics seq2;
  printf "The initial population is %d.\n" initial_pop;
  printf "The maximum population is %d.\n" max_pop;
  printf "\n%!";
  seq1, seq2

(* -------------------------------------------------------------------------- *)

(* Scenario execution. *)

(* [EXECUTE(seq, M)] runs the instruction sequence [seq] on the
   state [s] using the operations provided by the module [M]. *)

#define EXECUTE(seq, M) \
  for i = 0 to Array.length seq - 1 do \
    match Array.unsafe_get seq i with \
    | IAdd x -> \
        M.add s x \
    | IAddAbsent x -> \
        M.add_absent s x \
    | IRemove x -> \
        M.remove s x \
    | IMem x -> \
        ignore (M.mem s x) \
  done

(* [BENCHMARK(candidate, scenario, M)] expands to a benchmark whose name is
   [name candidate], obeying [scenario], using the operations provided by
   the module [M]. *)

#define BENCHMARK(candidate, M) \
( \
  let seq1, seq2 = scenario in \
  let basis = Array.length seq2 \
  and name = name candidate \
  and run () = \
    let s = M.create () in \
    EXECUTE(seq1, M); \
    fun () -> \
      EXECUTE(seq2, M) \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

(* [BENCHMARKS(NAME, SCENARIO)] expands to a list of benchmarks which obey
   [SCENARIO]. [NAME] is the name of the benchmark itself. *)

#define BENCHMARKS(NAME, SCENARIO) \
  let name candidate = \
    sprintf "%s (n = %d, u = %d) (%s)" NAME n u candidate in \
  let scenario = SCENARIO in \
  [ \
    (* BENCHMARK("Set", Set); *) \
    (* BENCHMARK("Baby.W.Set", BabyWSet); *) \
    BENCHMARK("Hashtbl", Hashtbl); \
    BENCHMARK("HashMap", HashMap); \
    BENCHMARK("HashSet", HashSet); \
    (* BENCHMARK("HectorHashSet", HashSet); *) \
  ]

(* [PROMOTE(NAME, F)] requires [F] to be a function of type [int -> recipes],
   mapping an integer [n] to a benchmark scenario. [NAME] is the name of this
   benchmark. The macro produces a new definition of [F] at type
   [int -> benchmark list]. *)

#define PROMOTE(NAME, F) \
  let F n : B.benchmark list = \
    printf "Scenario: %s\n" NAME; \
    let u = 10 * n in \
    let recipes = F n in \
    let scenario = choose_scenario u recipes in \
    BENCHMARKS(NAME, scenario)

(* -------------------------------------------------------------------------- *)

(* Specific recipes. *)

let either x y =
  if Random.bool() then x else y

#define ANY (either Absent Present)

(* Consecutive insertions: starting with an empty set, successively insert
   all integers from [0] to [n-1]. *)

(* In this benchmark, [u] is irrelevant. *)

let consecutive_insertions n : recipes =
  let recipe1 = empty
  and recipe2 = n, (fun i -> ITAdd (Key (2 * i))) in
  recipe1, recipe2

PROMOTE("consecutive insertions", consecutive_insertions)

(* Random insertions: starting with an empty set, insert [n] random
   integers. *)

let random_insertions n : recipes =
  let recipe1 = empty
  and recipe2 = n, (fun _ -> ITAdd Random) in
  recipe1, recipe2

PROMOTE("random insertions", random_insertions)

(* Random absent insertions: starting with an empty set, insert [n]
   random integers, which are not present already, and use
   [add_absent]. *)

let random_absent_insertions n : recipes =
  let recipe1 = empty
  and recipe2 = n, (fun _ -> ITAddAbsent) in
  recipe1, recipe2

PROMOTE("random absent insertions", random_absent_insertions)

(* Random deletions: starting with a set of cardinal [2 * n], remove [n]
   elements in a random order. *)

let random_deletions n : recipes =
  let recipe1 = 2 * n, (fun _ -> ITAdd Absent)
  and recipe2 =     n, (fun _ -> ITRemove Present) in
  recipe1, recipe2

PROMOTE("random deletions", random_deletions)

(* Random insertions and deletions: starting with a set of cardinal [n],
   perform [n] insertions or deletions (half of each). *)

let random_insertions_deletions n : recipes =
  let recipe1 = n, (fun _ -> ITAdd Absent)
  and recipe2 = n, (fun _ -> either (ITAdd Absent) (ITRemove Present)) in
  recipe1, recipe2

PROMOTE("random insertions and deletions", random_insertions_deletions)

(* Random lookups: starting with a set of cardinal [n], perform [n]
   lookups, including lookups of absent keys and lookups of present
   keys (half of each). *)

let random_lookups n : recipes =
  let recipe1 = n, (fun _ -> ITAdd Absent)
  and recipe2 = n, (fun _ -> ITMem ANY) in
  recipe1, recipe2

PROMOTE("random lookups", random_lookups)

(* A bit of everything: random insertions, deletions, and lookups. *)

let everything n : recipes =
  let recipe1 = empty
  and recipe2 = n, fun i ->
    let c = Random.int 100 in
    if i < n/2 then
      (* During a first phase, we perform more insertions, and a few
         deletions and lookups. *)
      if c < 80 then ITAdd ANY
      else if c < 90 then ITRemove ANY
      else ITMem ANY
    else
      (* During a second phase, we perform more lookups, and a few
         insertions and deletions. *)
      if c < 80 then ITMem ANY
      else if c < 90 then ITRemove ANY
      else ITAdd ANY
  in
  recipe1, recipe2

PROMOTE("a bit of everything", everything)

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let int (benchmarks : int -> B.benchmark list) : Arg.spec =
  Arg.Int (fun n -> run (benchmarks n))

let () =
  Arg.parse [
    "--consecutive-insertions", int consecutive_insertions, "";
    "--random-deletions", int random_deletions, "";
    "--random-insertions", int random_insertions, "";
    "--random-absent-insertions", int random_absent_insertions, "";
    "--random-insertions-deletions", int random_insertions_deletions, "";
    "--random-lookups", int random_lookups, "";
    "--everything", int everything, "";
  ] (fun _ -> ()) "Invalid usage"
