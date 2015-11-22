.PHONY: all clean install dist

VERSION=0.1
PACKAGE=hjc-$(VERSION)

OCAMLBUILD=ocamlbuild -use-ocamlfind

all:
	$(OCAMLBUILD) hjc.native
	cp hjc.native hjc

clean:
	$(OCAMLBUILD) -clean


install:
	cp hjc.native /usr/local/bin/hjc

dist:
	rm -fr $(PACKAGE)
	mkdir -p $(PACKAGE)
	cp *.ml configure _tags Makefile.in README.md $(PACKAGE)
	tar cvfz $(PACKAGE).tar.gz $(PACKAGE)
