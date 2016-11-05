test: errors1.output errors2.output
	diff errors1.output-goal errors1.output
	diff errors2.output-goal errors2.output

errors1.output: ../../bin/checklink-persistent-errors
	../../bin/checklink-persistent-errors errors1.txt 3 > $@

errors2.output: ../../bin/checklink-persistent-errors
	../../bin/checklink-persistent-errors errors2.txt 2 > $@

clean:
	rm -f errors1.output errors2.output
