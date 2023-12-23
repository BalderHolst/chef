#!/bin/sh
cargo run -- cook "examples/$1.rcp" --dot > "examples/$1.output.dot" &&
    echo "recorded."
cargo test
