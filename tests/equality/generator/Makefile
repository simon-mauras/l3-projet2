all:
	for i in `seq 2 9`; do echo $$i | ocaml simple_sudoku.ml > simple_sudoku-$$i.equ; done
	for s in *.in; do ocaml sudoku.ml < $${s%%.*}.in > $${s%%.*}.equ; done
clean:
	rm *.equ
