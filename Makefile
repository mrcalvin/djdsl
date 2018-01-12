TCLSH=tclsh
NSFSRC=/Users/ssoberni/Documents/dev/nsf

tutorial.src : tutorial.tcl
# echo "::proc apply args { puts [string trim [regsub -line -all {^[ \t][ \t]} [lindex \$$args 2] {}]]; puts [string trim [regsub -line -all {^[ \t][ \t]} [lindex \$$args 3] {}]]; }; source $<" | $(TCLSH) > $@
	$(TCLSH) $< --print > $@
tutorial.adoc : tutorial.src
	$(TCLSH) $(NSFSRC)/apps/utils/source-doc-beautifier.tcl -notitle $<
	mv tutorial.txt $@

test: loadscript.tcl
	$(TCLSH) tests/all.tcl \
		-loadfile loadscript.tcl \
		$(TESTFLAGS)

all: tutorial.adoc
