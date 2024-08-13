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
	@ $(MAIN) --add 2000000 2>&1 | tee log/add.log
	@ echo
	@ $(MAIN) --addrem 2000000 2>&1 | tee log/addrem.log
