test:
	dune build @install @runtest @src/benchmark/benchmarks

build:
	dune build

clean:
	rm -rf _build

benchmark:
	python src/benchmark/test.py

doc:
	dune build @doc
