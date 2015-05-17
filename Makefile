CC := ocamlbuild
SRC := src
CFLAGS := -tag thread -use-menhir -r -I $(SRC)

.PHONY: all mproper

all: main

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native
	mv $@.native resol

prof:
	$(CC) $(CFLAGS) $(SRC)/main.p.native
	mv main.p.native resol.p

clean:
	$(CC) -clean

doc:
	$(CC) $(CFLAGS) manual.docdir/index.html
