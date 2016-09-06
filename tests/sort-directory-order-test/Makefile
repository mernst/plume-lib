test: lines1.output
	diff lines1.output-goal lines1.output

lines1.output: ../../bin/sort-directory-order
	../../bin/sort-directory-order lines1.txt > $@

clean:
	rm -f lines1.output
