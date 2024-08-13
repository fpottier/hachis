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
end

(* We use -1 and -2 as sentinels. Our generators must be careful not
   to produce these values. *)
module S = struct type t = int let void = (-1) let tomb = (-2) end

module O = Hachis.HashSet.Make(A)(S)(V)

module H = struct
  open Hashtbl.Make(V)
  let[@inline] create () = create 128
  let[@inline] add s x = add s x ()
  let remove = remove
end

module VSet =
  Set.Make(V)

(* -------------------------------------------------------------------------- *)

(* Each benchmark is defined as a macro (not a higher-order function)
   because we want to benchmark realistic client code, where calls to
   library functions are inlined (when possible). *)

(* -------------------------------------------------------------------------- *)

(* Sequential insertions. *)

#define SEQADD(n, candidate, create, add) \
( \
  let basis = n \
  and name = sprintf "add (consecutive data, n = %d) (%s)" n candidate \
  and run () () = \
    let s = create () in \
    for i = 0 to n-1 do \
      let x = i in \
      ignore (add s x) \
    done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

(* -------------------------------------------------------------------------- *)

(* Random insertions. *)

(* We make sure that all benchmarks use the same random data. *)

let randadd_data =
  let data = ref [||] in
  fun n u ->
    if Array.length !data <> n then
      data := Array.init n (fun _i -> Random.int u);
    !data

#define RANDADD(n, u, candidate, create, add) \
( \
  let basis = n \
  and name = sprintf "add (random data, n = %d, u = %d) (%s)" n u candidate \
  and run () () = \
    let s = create () in \
    let data = randadd_data n u in \
    for i = 0 to n-1 do \
      let x = data.(i) in \
      ignore (add s x) \
    done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let adds n =
  let u = n in
  [
    SEQADD(n, "Hashtbl", H.create, H.add);
    SEQADD(n, "FlatHashSet", O.create, O.add);
    RANDADD(n, u, "Hashtbl", H.create, H.add);
    RANDADD(n, u, "FlatHashSet", O.create, O.add);
  ]

(* -------------------------------------------------------------------------- *)

(* Random insertions and deletions. *)

(* We want more than half of the operations to be insertions,
   because otherwise the hash set remains very small. *)

let[@inline] choose_insertion () =
  Random.int 100 < 80

let addrem_data =
  let data = ref [||] in
  fun n u ->
    if Array.length !data <> n then begin
      let present = ref VSet.empty
      and insertions = ref 0
      and pop = ref 0
      and maxpop = ref 0 in
      data := Array.init n (fun _i ->
          if VSet.is_empty !present || choose_insertion() then begin
            (* Insertion. *)
            let x = Random.int u in
            present := VSet.add x !present;
            incr insertions;
            incr pop;
            if !maxpop < !pop then maxpop := !pop;
            x
          end
          else begin
            (* Deletion, encoded as [-(x+1)]. *)
            let x = VSet.choose !present in
            present := VSet.remove x !present;
            decr pop;
            -(x+1)
          end
      );
      printf "add/rem: %d insertions, %d deletions, max population %d\n%!"
        !insertions (n - !insertions) !maxpop
    end;
    !data

#define ADDREM_CORE(n, u, create, add, remove) \
    let s = create () in \
    let data = addrem_data n u in \
    for i = 0 to n-1 do \
      let x = data.(i) in \
      if x >= 0 then \
        ignore (add s x) \
      else \
        let x = -x-1 in \
        ignore (remove s x) \
    done

#define ADDREM(n, u, candidate, create, add, remove) \
( \
  let basis = n \
  and name = sprintf "add/rem (n = %d, u = %d) (%s)" n u candidate \
  and run () () = \
    ADDREM_CORE(n, u, create, add, remove) \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let addrems n =
  let u = n in
  [
    ADDREM(n, u, "Hashtbl", H.create, H.add, H.remove);
    ADDREM(n, u, "FlatHashSet", O.create, O.add, O.remove);
  ]

let print_addrem_histogram n =
  let u = n in
  ADDREM_CORE(n, u, O.create, O.add, O.remove);
  print_string (O.statistics s);
  flush stdout

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let add, addrem =
  ref 0, ref 0

let addrem_histogram =
  ref false

let () =
  Arg.parse [
    "--add", Arg.Set_int add, " <n> Benchmark add";
    "--addrem", Arg.Set_int addrem, " <n> Benchmark add/rem";
    "--addrem_histogram", Arg.Set addrem_histogram, " Show add/rem histogram";
  ] (fun _ -> ()) "Invalid usage"

let possibly n (benchmarks : int -> B.benchmark list) =
  if n > 0 then run (benchmarks n)

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  if !addrem_histogram then print_addrem_histogram !addrem;
  possibly !add adds;
  possibly !addrem addrems;
  ()
