# YSC2229 Examples

Various examples of using YSC2229 libraries for writing your own
programs. The libraries are delivered as a standalone package
available for building and using from

https://github.com/ilyasergey/ysc2229-part-one

Please, consult the lecture notes for the theoretical material.:

https://ilyasergey.net/YSC2229

## Building the project

As this project depends on `ysc2229-part-one`, make sure the latter
one is installed via `opam`, as described in the `README.md` of the
[corresponding project](https://github.com/ilyasergey/ysc2229-part-one).

## Project Structure 

This project consist of the following folders:

* `lib` contains the libraries that build upon the modules from
  `ysc2229-part-one`, and serve as libraries for the runnable of this
  project. It also contains the `dune` descriptor for building the
  libraries.

* `runners` contains multiple examples of executables that rely both
  on local libraries (from `lib`), as well as the descriptor for
  compiling the runnable binaries. The corresponding files contain the
  comments with explanations of their ket components and intended
  functionality.

## Running REPL for the project

TODO: explain how utop works

## Essential configuration files

In order to have the smooth experience when working with the project,
a number of infrastructure files are provided. Sometime they need to
be modifier in order to add, e.g., new executables, but most of the
infrastructure is already fixed and will not need any enhancements as
the project grows. Here we explain some key elements.

### Building via dune

TODO: explain dune descriptors

### opam descriptor

TODO: explain the need in .opam

### Makefile

TODO: explain the need in Makefile

