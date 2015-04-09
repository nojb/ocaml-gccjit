OCAMLBUILD = ocamlbuild -classic-display
LIBGCCJITDIR = /usr/local/lib/gcc/5

all: lib_test/test.native

%.native:
	$(OCAMLBUILD) -lflags -ccopt,-L$(LIBGCCJITDIR) -tag-line "<$@>: use_gccjit" $@


clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
