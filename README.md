# DjDSL

[![License](https://img.shields.io/github/license/mrcalvin/djdsl)](https://opensource.org/licenses/MIT)
[![Build status](https://travis-ci.com/mrcalvin/djdsl.svg?branch=master)](https://travis-ci.com/mrcalvin/djdsl)
[![Latest release](https://img.shields.io/github/v/release/mrcalvin/djdsl?include_prereleases)](https://github.com/mrcalvin/djdsl/releases)
[![Supported platforms](https://img.shields.io/badge/platform-windows%20%7C%20macos%20%7C%20linux-orange.svg)](https://en.wikipedia.org/wiki/Cross-platform)

Develop variable and mixed, internal and external, domain-specific
software languages. `djdsl` is provided as a bundle of Tcl modules
(TMs).

https://github.com/mrcalvin/djdsl

This is the supplementary Web site to the book entitled [Variable Domain-specific Software Languages with DjDSL
(Springer, 2020)](https://doi.org/10.1007/978-3-030-42152-6) by [Stefan Sobernig](https://nm.wu.ac.at/en/sobernig).

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

## Quickstart

1. Download the self-contained executable (`djdslkit`) for your OS
    from the [releases](https://github.com/mrcalvin/djdsl/releases) page. 
2. Execute `djdslkit` from your terminal or command prompt to enter
   the DjDSL/Tcl shell:

On Linux:

```
$ chmod u+x djdslkit-latest-linux
$ ./djdslkit-latest-linux
```
On macOS:
```
$ chmod u+x djdslkit-latest-osx
$ ./djdslkit-latest-osx
```
On Windows:
```
C:\Your\User> djdslkit-latest-windows.exe
```

3. In the DjDSL/Tcl shell prompt, load a DjDSL component and proceed
by working through the various doctest examples or tutorials, e.g.:

```
% package require djdsl
```

or

```
% package require djdsl::v1e
```

then follow the [djdsl::v1e how-to](doc/v1e.adoc).

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
./configure --tclsh=/path/to/tclsh --with-tcl=no
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
