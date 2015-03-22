all:
	for i in `seq 1 10`; do echo $$i | ocaml gen1.ml > gen1-$$i.cnf; done
	for i in `seq 5 5 50`; do echo $$i | ocaml gen2.ml > gen2-$$i.cnf; done

clean:
	rm *.cnf
