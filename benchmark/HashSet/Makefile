# [make all] compiles this benchmark.

# Using --profile=release ensures that debug assertions are turned off.

.PHONY: all
all:
	@ dune build --profile=release

.PHONY: clean
clean:
	@ git clean -fdX

# ------------------------------------------------------------------------------

# The executable (previously built by dune).
# -machine requests machine-readable output.

EXE := \
  ../../_build/default/benchmark/HashSet/main.exe

# Abbreviations for thousands, millions, billions.

KILO=000
MEGA=000000
GIGA=000000000

# ------------------------------------------------------------------------------

# List of the benchmarks that should be executed.
SCHEMES := \
  absent-insertions-replace \
  absent-insertions-add-if-absent \
  deletions \
  lookups \
  lookups-tombstones \
  everything \

# ------------------------------------------------------------------------------

# [make bench] runs the benchmarks and produces *.out and *.pdf files.

SIZES := 100,1$(KILO),10$(KILO),100$(KILO),1$(MEGA),3$(MEGA),10$(MEGA),30$(MEGA)

.PHONY: bench
bench: all
	@ for S in $(SCHEMES) ; do \
	    prun \
	      -runs 1 \
	      -output $$S.out \
	      -prog "$(EXE) -machine" \
	      -candidate Hashtbl,HashMap,HashSet \
	      -scheme $$S \
	      -k $(SIZES) \
	    ; \
	    pplot scatter \
	      --yzero --xlog -x k -y nanos \
	      -legend-pos topleft \
	      -series candidate \
	      -input $$S.out \
	      -output $$S.pdf \
	    ; \
	    open $$S.pdf ; \
	  done

# ------------------------------------------------------------------------------

# Comparing [replace] and [add_if_absent] (on HashSet only).

.PHONY: compare-insertion-functions
compare-insertion-functions: all
	@ prun \
	      -runs 1 \
	      -output comparing-insertion-functions.out \
	      -prog "$(EXE) -machine" \
	      -candidate HashSet \
	      -scheme absent-insertions-replace,absent-insertions-add-if-absent \
	      -k $(SIZES)
	@ pplot scatter \
	      --yzero --xlog -x k -y nanos \
	      -legend-pos topleft \
	      -series scheme \
	      -input comparing-insertion-functions.out \
	      -output comparing-insertion-functions.pdf
	@ open comparing-insertion-functions.pdf

# ------------------------------------------------------------------------------

# [make one] runs one benchmark
# at one fixed size
# for one candidate implementation
# with human-readable output.

# Benchmark name.
S := lookups

# Size of benchmark scenario.
K := 2000000

# Candidate.
C := HashSet

.PHONY: one
one: all
	@ mkdir -p log
	@ $(EXE) -candidate $(C) -scheme $(S) -k $(K) 2>&1 | tee log/$$B.log
