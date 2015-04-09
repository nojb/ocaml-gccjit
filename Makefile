OCAMLBUILD = ocamlbuild -classic-display

all:
	$(OCAMLBUILD) lib/gccjit.cma

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
