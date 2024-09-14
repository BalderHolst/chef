use std::fs;
use std::process::ExitCode;

use clap::Parser;
use factorio_blueprint as fb;

mod cli;
mod gui;

fn main() -> ExitCode {
    let mut opts = cli::Opts::parse();

    if let Ok(p) = fs::read_to_string(&opts.blueprint) {
        opts.blueprint = p;
    }

    match fb::BlueprintCodec::decode_string(&opts.blueprint) {
        Ok(container) => gui::run(container),
        Err(e) => {
            eprintln!("Invalid blueprint string: {e}.");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
