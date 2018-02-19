TCLSH=tclsh
NSFSRC=/Users/ssoberni/Documents/dev/nsf


%.html : %.adoc
	asciidoctor $<

%.src : %.tcl
	$(TCLSH) $< --print > $@
%.adoc : %.src
	$(TCLSH) $(NSFSRC)/apps/utils/source-doc-beautifier.tcl -notitle $<
	mv $(basename $<).txt $@

%.tm : %.tcl loadscript.tcl
	$(TCLSH) $< \
		-loadfile loadscript.tcl

test: loadscript.tcl
	$(TCLSH) tests/all.tcl \
		-loadfile loadscript.tcl \
		$(TESTFLAGS)

all: tutorial.adoc
