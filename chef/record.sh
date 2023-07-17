#!/bin/sh
cargo run -- cook "examples/$1.rcp" --dot > "example_outputs/$1.dot" &&
    echo "recorded."
