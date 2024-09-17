# [make all] compiles this benchmark.

# Using --profile=release ensures that debug assertions are turned off.

.PHONY: all
all:
	@ dune build --profile=release

.PHONY: clean
clean:
	@ git clean -fdX

# The executable (built on the fly by dune).

MAIN := \
  dune exec --profile=release \
  --display=quiet --no-print-directory \
  ./main.exe -- \

# The executable (previously built by dune).

EXE := \
  ../../_build/default/benchmark/HashSet/main.exe

# [make bench] runs the benchmarks.

# Size of benchmark scenario.
K := 2000000

# List of the benchmarks that should be executed.
SCHEMES := \
  absent-insertions-replace \
  absent-insertions-add-absent \
  absent-insertions-add-if-absent \
  deletions \
  lookups \
  lookups-tombstones \
  everything \

# List of the candidates that should be executed.
CANDIDATES := \
  Hashtbl \
  HashMap \
  HashSet \

.PHONY: bench
bench: all
	@ mkdir -p log
	@ for S in $(SCHEMES) ; do \
	    for C in $(CANDIDATES) ; do \
	      $(MAIN) -candidate $$C -scheme $$S -k $(K) 2>&1 | tee log/$$B.log ; \
	      echo ; \
	    done; \
	  done

# Abbreviations for thousands, millions, etc.

KILO=000
MEGA=000000
GIGA=000000000

record: all
	@ for S in $(SCHEMES) ; do \
	    prun \
	      -runs 1 \
	      -output $$S.out \
	      -prog "$(EXE) -machine" \
	      -candidate Hashtbl,HashMap,HashSet \
	      -scheme $$S \
	      -k 1$(KILO),10$(KILO),100$(KILO),1$(MEGA),3$(MEGA),10$(MEGA),30$(MEGA),100$(MEGA) \
	    ; \
	  done

plot:
	@ for S in $(SCHEMES) ; do \
	    pplot scatter \
	      --yzero --xlog -x k -y nanos \
	      -legend-pos topleft \
	      -series candidate \
	      -input $$S.out \
	      -output $$S.pdf \
	    ; \
	  done

# ------------------------------------------------------------------------------

# Comparing [replace], [add_absent], and [add_if_absent].

.PHONY: compare-insertion-functions
compare-insertion-functions: all
	@ prun \
	      -runs 1 \
	      -output comparing-insertion-functions.out \
	      -prog "$(EXE) -machine" \
	      -candidate HashSet \
	      -scheme absent-insertions-add-absent,absent-insertions-replace,absent-insertions-add-if-absent \
	      -k 1$(KILO),10$(KILO),100$(KILO),1$(MEGA),3$(MEGA),10$(MEGA),30$(MEGA)
	@ pplot scatter \
	      --yzero --xlog -x k -y nanos \
	      -legend-pos topleft \
	      -series scheme \
	      -input comparing-insertion-functions.out \
	      -output comparing-insertion-functions.pdf
	@ open comparing-insertion-functions.pdf
