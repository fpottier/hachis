(******************************************************************************)
(*                                                                            *)
(*                                   Hachis                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Printf

module B = Common.Benchmark
module C = Common.Command

(* -------------------------------------------------------------------------- *)

(* A fixed random seed. *)

let () =
  Random.init 42

(* -------------------------------------------------------------------------- *)

(* [k] is the desired number of operations in the scenario. *)

let k : unit -> int =
  C.int "-k" 1000 "set desired number of operations"

(* -------------------------------------------------------------------------- *)

(* [quota] is the time quota alloted to one benchmark. *)

(* let quota : unit -> string = *)
(*   string "-quota" "5.0s" "set time quota" *)

(* We adapt [quota] based on [k]. *)

let quota () =
  let k = k() in
  if k <= 1_000_000 then
    "2.0s"
  else if k <= 3_000_000 then
    "6.0s"
  else if k <= 10_000_000 then
    "20.0s"
  else if k <= 30_000_000 then
    "60.0s"
  else if k <= 100_000_000 then
    "200.0s"
  else
    "600.0s"

(* -------------------------------------------------------------------------- *)

(* [candidate] is the candidate implementation whose performance must
   be measured. *)

let candidate : unit -> string =
  C.mandatory_string "-candidate" "choose candidate implementation"

(* -------------------------------------------------------------------------- *)

(* This benchmark compares several implementations of a minimal HashSet API.  *)

(* In [Stdlib.Hashtbl], [replace] returns a result of type [unit],
   so we have to adopt the same API. *)

module type API = sig
  type element = int
  type set
  val create : unit -> set
  val replace : set -> element -> unit
  val add_absent : set -> element -> unit
  val add_if_absent : set -> element -> unit
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

(* We use -1 and -2 as sentinels. Our generators must be careful not
   to produce these values. *)
module S = struct type t = int let void = (-1) let tomb = (-2) end

(* Instantiate Hachis.HashSet. *)

module HashSet : API = struct
  include Hachis.HashSet.Make(V)(S)
  let[@inline] replace s x = ignore (replace s x)
  let[@inline] add_if_absent s x = ignore (add_if_absent s x)
end

(* Instantiate [Hachis.HashSet] using [Hector.IntArray]. *)

(* module HectorHashSet : API = struct *)
(*   include Hachis.HashSet.Make_(V)(S)(Hector.IntArray) *)
(*   let[@inline] replace s x = ignore (replace s x) *)
(* end *)

(* Instantiate [Hachis.HashMap] so as to respect [API]. *)

module HashMap : API = struct
  include Hachis.HashMap.Make(V)(S)(V)
  type element = key
  type set = map
  let[@inline] replace s x = ignore (replace s x x)
  let[@inline] add_absent s x = add_absent s x x
  let[@inline] add_if_absent s x = ignore (add_if_absent s x x)
end

(* Instantiate [Stdlib.Hashtbl] so as to respect [API]. *)

(* [add_absent] is implemented using [add]. *)

module Hashtbl : API = struct
  include Stdlib.Hashtbl.Make(V)
  type element = int
  type set = unit t
  let[@inline] create () = create 128
  let[@inline] replace s x = replace s x ()
  and[@inline] add_absent s x = add s x ()
  let[@inline] add_if_absent s x = if not (mem s x) then replace s x
end

(* Instantiate [Stdlib.Set] so as to respect [API]. *)

(* [add_absent] is implemented using [add]. *)

module Set : API = struct
  open Set.Make(V)
  type element = int
  type set = t ref
  let[@inline] create () = ref empty
  let[@inline] replace s x = (s := add x !s)
  let add_absent = replace
  let[@inline] remove s x = (s := remove x !s)
  let[@inline] mem s x = mem x !s
  let[@inline] add_if_absent s x = if not (mem s x) then replace s x
end

(* Instantiate [Baby.W.Set] so as to respect [API]. *)

(* [add_absent] is implemented using [add]. *)

module BabyWSet : API = struct
  open Baby.W.Set.Make(V)
  type element = int
  type set = t ref
  let[@inline] create () = ref empty
  let[@inline] replace s x = (s := add x !s)
  let add_absent = replace
  let[@inline] remove s x = (s := remove x !s)
  let[@inline] mem s x = mem x !s
  let[@inline] add_if_absent s x = if not (mem s x) then replace s x
end

(* We use the module [R] in the generation of benchmark scenarios. *)

(* It enriches the underlying set with a maximum population field.
   [reset_max_pop] sets the maximum population field to the current
   population. [get_max_pop] reads the maximum population field. *)

module R = struct
  open Hachis.HashSet.Make(V)(S)
  type t = { mutable max_pop: int; now: set }
  let create () =
    { max_pop = 0; now = create() }
  let replace s x =
    ignore (replace s.now x);
    let n = cardinal s.now in
    if s.max_pop < n then s.max_pop <- n
  let add_if_absent s x =
    if not (mem s.now x) then replace s x
  let remove s x =
    remove s.now x
  let mem s x =
    mem s.now x
  let cardinal s =
    cardinal s.now
  let choose s =
    assert (cardinal s > 0);
    choose s.now
  let reset_max_pop s =
    s.max_pop <- cardinal s
  let get_max_pop s =
    s.max_pop
end

(* -------------------------------------------------------------------------- *)

(* Each benchmark is defined as a macro (not a higher-order function)
   because we want to benchmark realistic client code, where calls to
   library functions are inlined (when possible). *)

(* -------------------------------------------------------------------------- *)

(* Scenario generation. *)

type key =
  V.t

(* An instruction type. *)

type argument =
  | Key of key (* a specific key *)
  | Absent     (* any currently absent key *)
  | Present    (* any currently present key *)

type itype =
  | ITReplace of argument
  | ITAddAbsent of argument
  | ITAddIfAbsent of argument
  | ITRemove of argument
  | ITMem of argument

(* A concrete instruction. *)

type instruction =
  | IReplace of key
  | IAddAbsent of key
  | IAddIfAbsent of key
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

(* [choose_key a s] randomly chooses an integer value inside or
   outside of the set [s], depending on [a]. If it must be chosen
   outside of [s], then it is drawn randomly out of a very large
   range. *)

let rec choose_key (a : argument) s : key =
  match a with
  | Key x ->
      x
  | Absent ->
      let x = Random.int (1 lsl 29) in
      if R.mem s x then
        choose_key a s (* retry *)
      else
        x
  | Present ->
      if R.cardinal s = 0 then
        (* The request cannot be honored. Never mind. *)
        choose_key Absent s
      else
        R.choose s

(* [choose_instruction s it] randomly chooses a concrete instruction
   that corresponds to the instruction type [it]. This choice depends
   on the current state [s]. Furthermore, as a result of this choice,
   [s] is updated. *)

let choose_instruction s (it : itype) : instruction =
  match it with
  | ITReplace a ->
      let x = choose_key a s in
      ignore (R.replace s x);
      IReplace x
  | ITAddAbsent a ->
      assert (a <> Present);
      let x = choose_key a s in
      if R.mem s x then begin
        eprintf "Key %d is already present: cannot use add_absent.\n%!" x;
        assert false
      end;
      ignore (R.replace s x);
      IAddAbsent x
  | ITAddIfAbsent a ->
      let x = choose_key a s in
      ignore (R.add_if_absent s x);
      IAddIfAbsent x
  | ITRemove a ->
      let x = choose_key a s in
      R.remove s x;
      IRemove x
  | ITMem a ->
      let x = choose_key a s in
      IMem x

(* [choose_sequence s r] randomly chooses an instruction sequence
   that obeys the recipe [r]. *)

let choose_sequence s (r : recipe) : sequence =
  let n, (it : int -> itype) = r in
  Array.init n @@ fun i ->
    choose_instruction s (it i)

(* [print_statistics] prints statistics about a sequence of instructions. *)

let print_statistics (seq : sequence) =
  let insert, remove, mem = ref 0, ref 0, ref 0 in
  Array.iter (fun instruction ->
    match instruction with
    | IReplace   _   -> incr insert
    | IAddAbsent _   -> incr insert
    | IAddIfAbsent _ -> incr insert
    | IRemove    _   -> incr remove
    | IMem       _   -> incr mem
  ) seq;
  eprintf "This scenario has %d insertions, %d deletions, %d lookups.\n%!"
    !insert !remove !mem

(* [choose_scenario (r1, r2)] randomly chooses a scenario that begins with
   an empty set as the initial state and obeys the pair of recipes [(r1, r2)].
   The recipe [r1] is used to generate a first sequence of instructions
   (initialization). The recipe [r2] is used to generate a second sequence of
   instructions (use). *)

let choose_scenario (r1, r2 : recipes) : scenario =
  let s = R.create() in
  let seq1 = choose_sequence s r1 in
  R.reset_max_pop s;
  let initial_pop = R.get_max_pop s in
  let seq2 = choose_sequence s r2 in
  let max_pop = R.get_max_pop s in
  print_statistics seq2;
  eprintf "The initial population is %d.\n" initial_pop;
  eprintf "The maximum population is %d.\n" max_pop;
  eprintf "\n%!";
  seq1, seq2

(* -------------------------------------------------------------------------- *)

(* Scenario execution. *)

(* [EXECUTE(seq, M)] runs the instruction sequence [seq] on the
   state [s] using the operations provided by the module [M]. *)

#def EXECUTE(seq, M)
  for i = 0 to Array.length seq - 1 do
    match Array.unsafe_get seq i with
    | IReplace x ->
        M.replace s x
    | IAddAbsent x ->
        M.add_absent s x
    | IAddIfAbsent x ->
        M.add_if_absent s x
    | IRemove x ->
        M.remove s x
    | IMem x ->
        ignore (M.mem s x)
  done
#enddef

(* [BENCHMARK(M)] expands to a benchmark whose name is [name], obeying
   [scenario], using the operations provided by the module [M]. *)

#def BENCHMARK(M)
(
  let seq1, seq2 = scenario in
  let basis = Array.length seq2
  and quota = quota()
  and run () =
    let s = M.create () in
    EXECUTE(seq1, M);
    fun () ->
      EXECUTE(seq2, M)
  in
  B.benchmark ~name ~quota ~basis ~run
)
#enddef

(* [CBENCHMARK(c)] expands to a benchmark whose name is [name], obeying
   [scenario], using the operations provided by candidate [c]. *)

#def CBENCHMARK(c)
(
  match c with
  | "Set" ->
      BENCHMARK(Set)
  | "Baby.W.Set" ->
      BENCHMARK(BabyWSet)
  | "Hashtbl" ->
      BENCHMARK(Hashtbl)
  | "HashMap" ->
      BENCHMARK(HashMap)
  | "HashSet" ->
      BENCHMARK(HashSet)
  (* | "HectorHashSet" -> *)
  (*     BENCHMARK(HectorHashSet) *)
  | c ->
      eprintf "Error: unknown candidate (%s).\n%!" c;
      exit 1
)
#enddef

(* -------------------------------------------------------------------------- *)

(* [PROMOTE(NAME, F)] requires [F] to be a function of type [unit -> recipes].
   [NAME] is the name of this benchmark. The macro produces a new definition
   of [F] at type [unit -> benchmark]. *)

#def PROMOTE(NAME, F)
  let F () : B.benchmark =
    eprintf "Scenario: %s\n%!" NAME;
    let recipes = F () in
    let scenario = choose_scenario recipes in
    let c = candidate() in
    let name = sprintf "%s (%s)" NAME c in
    CBENCHMARK(c)
#enddef

(* -------------------------------------------------------------------------- *)

(* Specific recipes. *)

let either x y =
  if Random.bool() then x else y

#define ANY (either Absent Present)

(* [tombstones k] constructs a set of cardinal roughly [k] where both
   insertions and deletions have been performed. Thus, this set contains
   tombstones. *)

let tombstones k : recipe =
  2*k, fun i ->
    if i < k then ITReplace Absent
    else either (ITReplace Absent) (ITRemove Present)

(* Insert [k] absent keys using [replace]. *)

let absent_insertions_replace () : recipes =
  let k = k() in
  let recipe1 = empty
  and recipe2 = k, (fun i -> ITReplace (Key (2 * (i + 1)))) in
  recipe1, recipe2

PROMOTE("absent insertions (replace)", absent_insertions_replace)

(* Insert [k] absent keys using [add_absent]. *)

let absent_insertions_add_absent () : recipes =
  let k = k() in
  let recipe1 = empty
  and recipe2 = k, (fun i -> ITAddAbsent (Key (2 * (i + 1)))) in
  recipe1, recipe2

PROMOTE("absent insertions (add_absent)", absent_insertions_add_absent)

(* Insert [k] absent keys using [add_if_absent]. *)

let absent_insertions_add_if_absent () : recipes =
  let k = k() in
  let recipe1 = empty
  and recipe2 = k, (fun i -> ITAddIfAbsent (Key (2 * (i + 1)))) in
  recipe1, recipe2

PROMOTE("absent insertions (add_if_absent)", absent_insertions_add_if_absent)

(* Deletions: starting with a set of cardinal [2 * k], remove [k]
   elements in a random order. *)

let deletions () : recipes =
  let k = k() in
  let recipe1 = 2 * k, (fun _ -> ITReplace Absent)
  and recipe2 =     k, (fun _ -> ITRemove Present) in
  recipe1, recipe2

PROMOTE("deletions", deletions)

(* Lookups: starting with a set of cardinal [k], perform [k] lookups,
   including lookups of absent keys and lookups of present keys (half
   of each). *)

let lookups () : recipes =
  let k = k() in
  let recipe1 = k, (fun _ -> ITReplace Absent)
  and recipe2 = k, (fun _ -> ITMem ANY) in
  recipe1, recipe2

PROMOTE("lookups", lookups)

(* Lookups (with tombstones): starting with a set of cardinal roughly
   [k], where insertions and deletions have been performed, perform
   [k] lookups, including lookups of absent keys and lookups of
   present keys (half of each). *)

let lookups_tombstones () : recipes =
  let k = k() in
  let recipe1 = tombstones k
  and recipe2 = k, (fun _ -> ITMem ANY) in
  recipe1, recipe2

PROMOTE("lookups (with tombstones)", lookups_tombstones)

(* A bit of everything: random insertions, deletions, and lookups. *)

let everything () : recipes =
  let k = k() in
  let recipe1 = empty
  and recipe2 = k, fun i ->
    let c = Random.int 100 in
    if i < k/2 then
      (* During a first phase, we perform more insertions, and a few
         deletions and lookups. *)
      if c < 80 then ITReplace ANY
      else if c < 90 then ITRemove ANY
      else ITMem ANY
    else
      (* During a second phase, we perform more lookups, and a few
         insertions and deletions. *)
      if c < 80 then ITMem ANY
      else if c < 90 then ITRemove ANY
      else ITReplace ANY
  in
  recipe1, recipe2

PROMOTE("a bit of everything", everything)

(* -------------------------------------------------------------------------- *)

(* [scheme] is the name of the benchmark that we must run. *)

let scheme : unit -> string =
  C.mandatory_string "-scheme" "choose benchmark scheme"

let benchmark () : B.benchmark =
  match scheme() with
  | "absent-insertions-replace" ->
      absent_insertions_replace()
  | "absent-insertions-add-absent" ->
      absent_insertions_add_absent()
  | "absent-insertions-add-if-absent" ->
      absent_insertions_add_if_absent()
  | "deletions" ->
      deletions()
  | "lookups" ->
      lookups()
  | "lookups-tombstones" ->
      lookups_tombstones()
  | "everything" ->
      everything()
  | s ->
      eprintf "Error: unknown scheme (%s).\n%!" s;
      exit 1

(* -------------------------------------------------------------------------- *)

(* [machine] selects machine-readable or human-readable output. *)

let machine : unit -> bool =
  C.optional_flag "-machine" "request machine-readable output"

(* -------------------------------------------------------------------------- *)

(* Parse. *)

let () =
  C.parse()

(* Act. *)

let () =
  let drive = if machine() then B.drive_and_print else B.drive_and_display in
  let benchmark = benchmark() in
  drive benchmark
