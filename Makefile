OCAMLBUILD = ocamlbuild -classic-display

all:
	$(OCAMLBUILD) lib/gccjit.cma lib/gccjit.cmxa lib/gccjit.a lib/libgccjit_stubs.a lib/dllgccjit_stubs.so

square:
	$(OCAMLBUILD) lib_test/square.byte

clean:
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) -docflags -colorize-code,-css-style,style.css doc/api.docdir/index.html
	cp doc/style.css api.docdir/

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp api.docdir/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

.PHONY: clean doc
