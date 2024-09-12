use std::process::ExitCode;

use clap::Parser;
use factorio_blueprint as fb;

mod gui;
mod cli;

fn main() -> ExitCode {
    let opts = cli::Opts::parse();
    match fb::BlueprintCodec::decode_string(&opts.blueprint) {
        Ok(container) => gui::run(container),
        Err(e) => {
            eprintln!("Invalid blueprint string: {e}.");
            return ExitCode::FAILURE;
        },
    }

    ExitCode::SUCCESS
}
