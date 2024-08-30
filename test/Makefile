# [make test] runs 2 tests in parallel, namely HashSet and HashMap.
# These tests do not terminate; they run forever.

# [make test] also executes [dune runtest],
# which runs the test in the subdirectory statistics/.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ dune runtest
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  HashSet \
	  HashMap \

.PHONY: clean
clean:
	@ git clean -fdX
