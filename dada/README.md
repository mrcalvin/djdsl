# djdsl::dada

Define and process variable internal DSL syntaxes based on internal
DSL patterns. `djdsl::dada` is provided as a Tcl module (TM).

https://github.com/mrcalvin/djdsl/dada

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

- Tcl 8.6
- NSF 2.1.0+

### Installing

A step by step series of examples that tell you how to get a development env running

Run configure

```
./configure --with-tcl=/path/to/tclConfig.sh --tclsh=/path/to/tclsh
```

Run make

```
make
```

Run tests

```
make test
```

Run shell

```
make shell
% package req djdsl::ctx
% namespace import ::djdsl::ctx::*
% nx::Class create MyDslBuilder -superclasses Builder
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
