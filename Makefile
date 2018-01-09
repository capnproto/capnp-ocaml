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

REPO=../opam-repository
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
