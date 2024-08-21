# [make all] compiles this benchmark.

.PHONY: all
all:
	@ dune build --profile release

# [make bench] runs the benchmarks.

# Using --profile release ensures that debug assertions are turned off.

MAIN := \
  dune exec --profile release \
  --display=quiet --no-print-directory \
  ./main.exe -- \

N := 2000000

BENCHMARKS := \
  consecutive-insertions \
  random-insertions \
  random-absent-insertions \
  random-deletions \
  random-insertions-deletions \
  random-lookups \
  everything \

.PHONY: bench
bench:
	@ mkdir -p log
	@ for B in $(BENCHMARKS) ; do \
	    $(MAIN) --$$B $(N) 2>&1 | tee log/$$B.log ; \
	    echo ; \
	  done
