#!/usr/bin/env bash
cargo run -- -v cook $1 -g tmp.svg -G tmp2.svg && bat $1 && eog tmp.svg ; bat $1
