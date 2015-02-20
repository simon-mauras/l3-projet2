CC := ocamlbuild
SRC := src
CFLAGS := -use-ocamlfind -tag thread -r -I $(SRC)

.PHONY: all mproper

all: main

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native
	mv $@.native resol

clean:
	$(CC) -clean

