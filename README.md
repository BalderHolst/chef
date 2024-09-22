# The Chef Language!

Behold... this unfinished monster of a project: *a language for expressing Factorio circuit networks*. Chef is a statically typed language that compiles into a blueprint string, ready to be pasted directly into Factorio!

## The Language

The chef syntax has gone through many iterations and is not completely finalized, but to get an idea, here is a counter implemented in chef:

```chef
// A simple counter block that counts from 1 to a limit repeatedly.
// `limit` is an integer block input
// `count` is an integer block output
block counter(limit: int) => (count: int) {

    // Variables in chef can be thought of connection points/nodes
    // that can be connected to other nodes directly or through an
    // operation
    let input: int(signal-C);
    let output: int(signal-C);

    // Add a constant 1 to the input
    input <<- 1;

    // Connect the `input` node to the `output` node only if
    // `input` has a value of less than `limit´
    //
    // This can also be thought of as adding a decider
    // combinator `input` and `output` being its conneciton
    // points.
    output <- ? input < limit <- input;

    // Also connect the two nodes directly
    input <- output;

    // Connect the output
    count <<- output;
}


// The "entry point" of a chef program. This takes no
// inputs, but may have outputs. Here we specify
// the that blueprint will output an integer on the
// `tank` signal.
block main() => (out: int(tank)) {

    // Instantiate a `counter` block with a limit of `10`
    out <<- counter(10);

}
```

The Chef language is constantly evolving as I learn from my mistakes, so I will not try to fully explain the syntax of the language here. Instead, take a look at the [examples](./examples).

### Code Generation
In Chef, you can include Python files just like any other Chef file.

```text
import "./some/script.py"
```

Importing a Python script runs it and treats its standard output as input Chef code. This provides a way to create blueprints based on outside sources like the internet or generate repetitive code. It works by running the provided python script and treating its standard output as chef code. **The provided python module can be used to generate chef code more ergonomically**, but it is not required. Using simple print statements to print out code will also work just fine.


## About the Repository

This repository has three main parts:
- [The Chef Compiler](#the-compiler)
- [A GUI program for inspecting blueprints](#chef-inspector)
- [A Barebones Python Module for Generating Chef Code](#code-generation)
- [Factorio Mod for Extracting Signals](#factorio-mod)

## The Compiler
The compiler is written in [Rust](https://www.rust-lang.org/) and contains modules for lexing, parsing, type checking, compiling, and placing combinators within a Factorio blueprint.

This is an interesting language to me because it does not compile to a sequence of instructions, but rather a graph. The graph contains nodes that can contain signals with either direct connections (wires) or operations between them. The compiler also contains a module for simulating how this graph behaves in Factorio used for testing and debugging.

Documentation for the internals of the compiler can be found [here](https://balderholst.github.io/chef/chef/).

## Chef Inspector
It is tedious and sometimes hard to verify the correctness of a blueprint, even when pasted into factorio. It is even harder to debug the wires if anything should go wrong. The Chef Inspector takes a blueprint string as an input (either directly or within a file) and draws the combinators. The UI is designed to make it easy to se what a combinator is doing and where its inputs and outputs are going at a glance. When compiling, add the `--gui` flag to open the output blueprint in the inspector.

## Factorio Mod
A simple Factorio mod is provided in this repository. It simply extracts signals from Factorio. This provides a way to use modded items and signals in types when writing Chef code. By default, only vanilla signals are allowed in types.
