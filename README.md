# The Chef Language!

Behold... this unfinished monster of a project: *a language for expressing Factorio circuit networks*. Chef is a statically typed language that compiles into a blueprint string, ready to be pasted directly into Factorio!

## About the Repository

This repository has three main parts:
- [The Chef Compiler](#the-compiler)
- [A Barebones Python Module for Generating Chef Code](#code-generation)
- [Factorio Mod for Extracting Signals](#factorio-mod)

## The Compiler
The compiler is written in [Rust](https://www.rust-lang.org/) and contains modules for lexing, parsing, type checking, compiling, and placing combinators within a Factorio blueprint.

This is an interesting language to me because it does not compile to a sequence of instructions, but rather a graph. The graph contains nodes that can contain signals with either direct connections (wires) or operations between them. The compiler also contains a module for simulating how this graph behaves in Factorio used for testing and debugging.

The Chef language is constantly evolving as I learn from my mistakes, so I will not try to explain the syntax of the language here. Instead, take a look at the [examples](chef-lang/examples).

Documentation for the internals of the compiler can be found [here](https://balderholst.github.io/chef/chef/).

### Code Generation

In Chef, you can include Python files just like any other Chef file.

```text
import "./some/script.py"
```

Importing a Python script runs it and treats its standard output as input Chef code. This provides a way to create blueprints based on outside sources like the internet or generate repetitive code. It works by running the provided python script and treating its standard output as chef code. **The provided python module can be used to generate chef code more ergonomically**, but it is not required. Using simple print statements to print out code will also work just fine.

## Factorio Mod
A simple Factorio mod is provided in this repository. It simply extracts signals from Factorio. This provides a way to use modded items and signals in types when writing Chef code. By default, only vanilla signals are allowed in types.
