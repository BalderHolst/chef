use std::fs;
use std::process::ExitCode;

use clap::Parser;
use factorio_blueprint as fb;

mod cli;
mod gui;

fn main() -> ExitCode {
    let opts = cli::Opts::parse();

    let mut blueprint = opts.blueprint.to_string();

    if let Ok(p) = fs::read_to_string(&blueprint) {
        blueprint = p;
    }

    match fb::BlueprintCodec::decode_string(&blueprint) {
        Ok(container) => gui::run(container),
        Err(e) => {
            eprintln!("Invalid blueprint string: {e}.");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
