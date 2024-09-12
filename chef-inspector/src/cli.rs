use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
pub struct Opts {
    pub blueprint: String,
}
