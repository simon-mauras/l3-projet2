all:
	for s in *.in; do ocaml sudoku.ml < $${s%%.*}.in > $${s%%.*}.diff; done
clean:
	rm *.diff
