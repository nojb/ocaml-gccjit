OCAMLBUILD = ocamlbuild -classic-display
LIBGCCJITDIR = /usr/local/lib/gcc/5

square: lib_test/square.native

%.native:
	$(OCAMLBUILD) -lflags -ccopt,-L$(LIBGCCJITDIR) -tag-line "<$@>: use_gccjit" $@

%.byte:
	$(OCAMLBUILD) -lflags -ccopt,-L$(LIBGCCJITDIR) -tag-line "<$@>: use_gccjit" $@

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

.PHONY: %.native %.byte clean doc
