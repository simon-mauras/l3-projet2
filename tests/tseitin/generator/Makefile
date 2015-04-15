all:
	for s in *.in; do ocaml sudoku.ml < $${s%%.*}.in > $${s%%.*}.tsei; done
clean:
	rm *.tsei
