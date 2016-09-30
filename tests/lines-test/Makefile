# TODO: Write more tests

test: lines-before-test lines-between-test lines-notbetween-test

lines-before-test: lines-before-1 lines-before-2
lines-before-1:
	../../bin/lines-before A input.txt | diff -u lines-before-1.goal -
lines-before-2:
	../../bin/lines-before B input.txt | diff -u lines-before-2.goal -

lines-between-test: lines-between-1 lines-between-2 lines-between-3 lines-between-4
lines-between-1:
	../../bin/lines-between B D input.txt | diff -u lines-between-1.goal -
lines-between-2:
	../../bin/lines-between --exclusive B D input.txt | diff -u lines-between-2.goal -
lines-between-3:
	../../bin/lines-between A E input.txt | diff -u lines-between-3.goal -
lines-between-4:
	../../bin/lines-between --exclusive A E input.txt | diff -u lines-between-4.goal -

lines-notbetween-test: lines-notbetween-1 lines-notbetween-2 lines-notbetween-3 lines-notbetween-4
lines-notbetween-1:
	../../bin/lines-notbetween B D input.txt | diff -u lines-notbetween-1.goal -
lines-notbetween-2:
	../../bin/lines-notbetween --inclusive B D input.txt | diff -u lines-notbetween-2.goal -
lines-notbetween-3:
	../../bin/lines-notbetween A E input.txt | diff -u lines-notbetween-3.goal -
lines-notbetween-4:
	../../bin/lines-notbetween --inclusive A E input.txt | diff -u lines-notbetween-4.goal -
