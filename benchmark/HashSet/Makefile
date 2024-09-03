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

# Time quota, in seconds.
QUOTA := 5.0s

# Size of benchmark scenario.
N := 2000000

# List of the benchmarks that should be executed.
BENCHMARKS := \
  consecutive-insertions \
  consecutive-absent-insertions \
  random-insertions \
  random-deletions \
  random-insertions-deletions \
  random-lookups \
  random-lookups-tombstones \
  everything \

.PHONY: bench
bench:
	@ mkdir -p log
	@ for B in $(BENCHMARKS) ; do \
	    $(MAIN) --quota $(QUOTA) --$$B $(N) 2>&1 | tee log/$$B.log ; \
	    echo ; \
	  done
