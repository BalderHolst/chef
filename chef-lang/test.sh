#!/usr/bin/env bash
cargo run -- cook -v $1 -g tmp.svg -G tmp2.svg && bat $1 && eog tmp.svg
