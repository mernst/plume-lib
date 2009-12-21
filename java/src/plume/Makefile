default %:
	cd .. && $(MAKE) $@

###########################################################################
### Automatically generated files
###

PLUMEBIN ?= ../../../bin
JAVA_CPP_PL ?= ${PLUMEBIN}/java-cpp.pl
# JAVA_CPP = java-cpp.pl $< -DDEFINEDVAR > $@
# This rule does not work with make 3.77, but does work with make 3.78.
JAVA_CPP ?= (rm -f $@; ${JAVA_CPP_PL} -DDEFINEDVAR $< 2>&1 > $@ && chmod -w $@)

MathMDE.java: MathMDE.java.jpp MathMDE-gcd.java.jpp MathMDE-modulus.java.jpp MathMDE-nonmodulus.java.jpp MathMDE-help.java.jpp MathMDE-helpend.java.jpp
	$(JAVA_CPP)
