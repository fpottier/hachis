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

.PHONY: bench
bench:
	@ mkdir -p log
	@ echo
	@ $(MAIN) --consecutive-insertions 2000000 2>&1 | tee log/consecutive-insertions.log
	@ echo
	@ $(MAIN) --random-insertions 2000000 2>&1 | tee log/random-insertions.log
	@ echo
	@ $(MAIN) --random-deletions 2000000 2>&1 | tee log/random-deletions.log
