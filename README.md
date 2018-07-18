This is the supplementary Web site to the book *Variability design
and implementation for domain-specific software languages*
(forthcoming) by [Stefan Sobernig](http://nm.wu.ac.at/en/sobernig).

The supplemental material includes the multi-DSL development system DjDSL.

* The following script files contain Tcl modules providing the main
   DjDSL components:
    * [`djdsl::lm`](lm.tcl): Define variable language models
      for DSL. See the [djdsl::lm how-to](doc/lm.adoc).
    * [`djdsl::ctx`](ctx.tcl): Define variable context conditions for
      language models. See the [djdsl::ctx how-to](doc/ctx.adoc).
    * [`djdsl::dada`](dada.tcl): Define variable *internal*  DSL syntaxes. See the [djdsl::dada how-to](doc/dada.adoc).
	* [`djdsl::opeg`](opeg.tcl): Define variable *external* DSL
	  syntaxes using composable parsing grammars (PEG). See the
      [djdsl::opeg ada how-to](doc/opeg.adoc).
	* [`djdsl:: v1e`](v1e.tcl): Define variability models for
      DSL-product lines. See the [djdsl::v1e how-to](doc/v1e.adoc).
* [doc](doc/) contains the doctests for the five main components
  (NX/Tcl modules) that form DjDSL (see how-tos above).
* [tutorials](tutorials/) contains important background material on
    internal DSL patterns in NX/Tcl, DSL development in NX/Tcl, and
    the application cases featured by the book.


