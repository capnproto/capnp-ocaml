all:
	jbuilder build --dev

test:
	jbuilder runtest --dev

clean:
	rm -rf _build

benchmark:
	python src/benchmark/test.py
