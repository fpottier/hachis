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

(* This benchmark compares several implementations of a minimal HashSet API,
   which (at this point) includes just [create], [add], and [remove]. *)

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

module HashSet = Hachis.HashSet.Make(A)(S)(V)

(* Instantiate Hachis.HashMap so as to respect the HashSet API. *)

module HashMap = struct
  open Hachis.HashMap.Make(A)(S)(V)(A)
  let create = create
  let[@inline] add s x = add s x x
  let remove = remove
end

(* Instantiate Stdlib.Hashtbl so as to respect the HashSet API. *)

module Hashtbl = struct
  open Stdlib.Hashtbl.Make(V)
  let[@inline] create () = create 128
  let[@inline] add s x = add s x ()
  let remove = remove
end

(* Instantiate Stdlib.Set so as to respect the HashSet API. *)

module Set = struct
  open Set.Make(V)
  let[@inline] create () = ref empty
  let[@inline] add s x = (s := add x !s)
  let[@inline] remove s x = (s := remove x !s)
end

(* Instantiate Baby.W.Set so as to respect the HashSet API. *)

module BabyWSet = struct
  open Baby.W.Set.Make(V)
  let[@inline] create () = ref empty
  let[@inline] add s x = (s := add x !s)
  let[@inline] remove s x = (s := remove x !s)
end

(* We use this Set module in the generation of benchmark scenarios. *)

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
  | ITRemove of argument

(* A concrete instruction is [add x] or [remove x]. *)

type instruction =
  | IAdd of key
  | IRemove of key

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
  | ITRemove a ->
      let x = choose_key a s u in
      R.remove s x;
      IRemove x

(* [choose_sequence s u r] randomly chooses an instruction sequence
   that obeys the recipe [r]. *)

let choose_sequence s u (r : recipe) : sequence =
  let n, (it : int -> itype) = r in
  Array.init n @@ fun i ->
    choose_instruction s u (it i)

(* [print_statistics] prints statistics about a sequence of instructions. *)

let print_statistics (seq : sequence) =
  let add, remove = ref 0, ref 0 in
  Array.iter (fun instruction ->
    match instruction with
    | IAdd    _ -> incr add
    | IRemove _ -> incr remove
  ) seq;
  printf "This scenario involves %d insertions and %d deletions.\n"
    !add !remove

(* [choose_scenario u (r1, r2)] randomly chooses a scenario that begins with
   an empty set as the initial state and obeys the pair of recipes [(r1, r2)].
   The recipe [r1] is used to generate a first sequence of instructions
   (initialization). The recipe [r2] is used to generate a second sequence of
   instructions (use). *)

let choose_scenario u (r1, r2 : recipe * recipe) : scenario =
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

(* [EXECUTE(seq, add, remove)] runs the instruction sequence [seq] on the
   state [s] using the operations [add] and [remove]. *)

#define EXECUTE(seq, add, remove) \
  for i = 0 to Array.length seq - 1 do \
    match Array.unsafe_get seq i with \
    | IAdd x -> \
        ignore (add s x) \
    | IRemove x -> \
        ignore (remove s x) \
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
    EXECUTE(seq1, M.add, M.remove); \
    fun () -> \
      EXECUTE(seq2, M.add, M.remove) \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

(* [BENCHMARKS(NAME, SCENARIO)] expands to a list of benchmarks which obey
   [SCENARIO]. [NAME] is the name of the benchmark itself. *)

#define BENCHMARKS(NAME, SCENARIO) \
  printf "Scenario: %s\n" NAME; \
  let name candidate = \
    sprintf "%s (n = %d, u = %d) (%s)" NAME n u candidate in \
  let scenario = SCENARIO in \
  [ \
    BENCHMARK("Set", Set); \
    BENCHMARK("Baby.W.Set", BabyWSet); \
    BENCHMARK("Hashtbl", Hashtbl); \
    BENCHMARK("HashSet", HashSet); \
    BENCHMARK("HashMap", HashMap); \
  ]

(* -------------------------------------------------------------------------- *)

(* Specific (pairs of) recipes. *)

let empty : recipe =
  0, (fun _ -> assert false)

(* Consecutive insertions: starting with an empty set, successively insert
   all integers from [0] to [n-1]. *)

(* In this benchmark, [u] is irrelevant. *)

let consecutive_insertions n u : scenario =
  let seq1 = empty
  and seq2 = n, (fun i -> ITAdd (Key (2 * i))) in
  choose_scenario u (seq1, seq2)

(* Random insertions: starting with an empty set, insert [n] random
   integers. *)

let random_insertions n u : scenario =
  let seq1 = empty
  and seq2 = n, (fun _ -> ITAdd Random) in
  choose_scenario u (seq1, seq2)

(* Random deletions: starting with a set of cardinal [2 * n], remove [n]
   elements in a random order. *)

let random_deletions n u : scenario =
  let seq1 = 2 * n, (fun _ -> ITAdd Absent)
  and seq2 =     n, (fun _ -> ITRemove Present) in
  choose_scenario u (seq1, seq2)

(* Random insertions and deletions: starting with a set of cardinal [n],
   perform [n] insertions or deletions (half of each). *)

let random_insertions_deletions n u : scenario =
  let seq1 = n, (fun _ -> ITAdd Absent)
  and seq2 = n, (fun _ -> if Random.bool() then ITAdd Absent else ITRemove Present) in
  choose_scenario u (seq1, seq2)

(* -------------------------------------------------------------------------- *)

(* Benchmarks. *)

let consecutive_insertions n : B.benchmark list =
  let u = 10 * n in
  BENCHMARKS("consecutive insertions", consecutive_insertions n u)

let random_insertions n : B.benchmark list =
  let u = 10 * n in
  BENCHMARKS("random insertions", random_insertions n u)

let random_deletions n : B.benchmark list =
  let u = 10 * n in
  BENCHMARKS("random deletions", random_deletions n u)

let random_insertions_deletions n : B.benchmark list =
  let u = 10 * n in
  BENCHMARKS("random insertions and deletions ", random_insertions_deletions n u)

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let int (benchmarks : int -> B.benchmark list) : Arg.spec =
  Arg.Int (fun n -> run (benchmarks n))

let () =
  Arg.parse [
    "--consecutive-insertions", int consecutive_insertions, "";
    "--random-deletions", int random_deletions, "";
    "--random-insertions", int random_insertions, "";
    "--random-insertions-deletions", int random_insertions_deletions, "";
  ] (fun _ -> ()) "Invalid usage"
