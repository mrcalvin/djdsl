# DjDSL

Define variable and mixed, internal and external, domain-specific
software languages. `djdsl` is provided as a bundle of Tcl modules
(TMs).

https://github.com/mrcalvin/djdsl

This is the supplementary Web site to the book *Variability design
and implementation for domain-specific software languages*
(forthcoming) by [Stefan Sobernig](http://nm.wu.ac.at/en/sobernig).

The supplemental material includes the multi-DSL development system DjDSL.

* The following script files contain Tcl modules providing the main
   DjDSL components:
    * [`djdsl::lm`](lm/): Define variable language models
      for DSL. See the [djdsl::lm how-to](doc/lm.adoc).
    * [`djdsl::ctx`](ctx/): Define variable context conditions for
      language models. See the [djdsl::ctx how-to](doc/ctx.adoc).
    * [`djdsl::dada`](dada/): Define variable *internal*  DSL syntaxes. See the [djdsl::dada how-to](doc/dada.adoc).
	* [`djdsl::opeg`](opeg/): Define variable *external* DSL
	  syntaxes using composable parsing grammars (PEG). See the
      [djdsl::opeg how-to](doc/opeg.adoc).
	* [`djdsl::v1e`](v1e/): Define variability models for
      DSL-product lines. See the [djdsl::v1e how-to](doc/v1e.adoc).
* [doc](doc/) contains the doctests for the five main components
  (NX/Tcl modules) that form DjDSL (see how-tos above).
* [tutorials](tutorials/) contains important background material on
    internal DSL patterns in NX/Tcl, DSL development in NX/Tcl, and
    the application cases featured by the book.

## Getting Started

These instructions will get you a copy of the project up and running
on your local machine for development and testing purposes. See
deployment for notes on how to deploy the project on a live system.

### Prerequisites

See the sub-projects READMEs to learn about the required dependencies.

### Installing

A step by step series of examples that tell you how to get a development env running

Run configure

```
./configure --with-tcl=/path/to/tclConfig.sh --tclsh=/path/to/tclsh
```

Run make

```
make build
```

Run tests

```
make test
```

Run shell

```
make shell
% package req djdsl
```

## Deployment

To install the built Tcl module into a Tcl module path recognised by
the targeted Tcl installation, run:

```
make install
```

## Built With

* [Next Scripting Framework (NSF)/ Next Scripting Language (NX)](https://next-scripting.org/) - The Tcl language extension used
* [spotoconf](https://chiselapp.com/user/stwo/repository/spotoconf/index) - configure, build, and installer system

## Authors

* **Stefan Sobernig** - *Initial work* - [WU Vienna](https://nm.wu.ac.at/en/sobernig)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
