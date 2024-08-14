# [make test] runs 2 tests in parallel, namely HashSet and HashMap.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  HashSet \
	  HashMap \

.PHONY: clean
clean:
	@ git clean -fdX
