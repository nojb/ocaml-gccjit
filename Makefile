OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind
CC = cc

generate:
	$(OCAMLBUILD) stubgen/gccjit_enum_generator.byte
	./gccjit_enum_generator.byte
	$(CC) gccjit_generated_enums.c -I `ocamlfind query ctypes` -I `ocamlfind query ctypes`/../ocaml
	./a.out > lib/gccjit_enums_gen.ml
	$(OCAMLBUILD) stubgen/gccjit_stub_generator.byte
	./gccjit_stub_generator.byte

all: generate
	$(OCAMLBUILD) lib/gccjit.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean generate
