test:
	jbuilder build --dev @install @runtest @src/benchmark/benchmarks

build:
	jbuilder build --dev

clean:
	rm -rf _build

benchmark:
	python src/benchmark/test.py

doc:
	jbuilder build @doc
