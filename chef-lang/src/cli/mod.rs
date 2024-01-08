//! The chef cli.

use clap::{Parser, Subcommand};
use terminal_size::Width;

/// Command line options for chef
#[derive(Debug, Parser)]
pub struct Opts {
    /// Do not give cooking advice
    #[arg(short, long)]
    pub(crate) no_advice: bool,

    /// Be verbose
    #[arg(short, long)]
    pub(crate) verbose: bool,

    #[command(subcommand)]
    pub(crate) command: Command,
}

#[cfg(test)]
impl Opts {
    pub fn new_test() -> Self {
        let command = Command::Cook(CookOpts {
            file: "dummy_file.rcp".to_string(),
            dot: false,
            graph: None,
            fgraph: None,
            verbose: false,
        });
        Self {
            no_advice: true,
            verbose: true,
            command,
        }
    }
}

/// Chef cli subcommands
#[derive(Debug, Subcommand)]
pub enum Command {
    /// Compile source code
    Cook(CookOpts),

    /// Simulate a chef program
    Simulate(SimulateOpts),

    /// Add signals to your project
    Add(AddOpts),
}

/// Options for the cli `cook` subcommand.
#[derive(Debug, clap::Args)]
pub struct CookOpts {
    /// File to compile.
    #[arg(required = true)]
    pub(crate) file: String,

    /// Output only a graph version of the output in 'dot' format.
    #[arg(short, long)]
    pub(crate) dot: bool,

    /// Output an svg to visualize the circuit.
    #[arg(short, long)]
    pub(crate) graph: Option<String>,

    /// Output an svg to visualize the factorio circuit connections.
    #[arg(short('G'), long)]
    pub(crate) fgraph: Option<String>,

    /// Be verbose
    #[arg(short('v'), long)]
    pub(crate) verbose: bool,
}

impl CookOpts {
    pub fn from_files(file: String) -> Self {
        Self {
            dot: false,
            file,
            graph: None,
            fgraph: None,
            verbose: false,
        }
    }
}

/// Options for the cli `add` subcommand.
#[derive(Debug, clap::Args, Clone)]
pub struct SimulateOpts {
    /// File to compile.
    #[arg(required = true)]
    pub(crate) file: String,

    /// Directory to output frames
    #[arg(short, long)]
    pub(crate) output: Option<String>,

    /// Number of iterations default is 10
    #[arg(short, long, default_value = "10")]
    pub(crate) iterations: usize,
}

/// Options for the cli `add` subcommand.
#[derive(Debug, clap::Args)]
pub struct AddOpts {
    #[command(subcommand)]
    pub(crate) command: AddCommand,
}

/// The cli `add` subcommand, used for adding signal files to the project.
#[derive(Debug, Subcommand)]
pub enum AddCommand {
    /// Add signals exported from game with the factorio mod
    Signals(AddSignalOpts),
}

/// TODO
#[derive(Debug, clap::Args)]
pub struct AddSignalOpts {}

/// Get the size of the current terminal that chef is running in.
fn get_term_width() -> Option<usize> {
    if let Some((Width(w), _)) = terminal_size::terminal_size() {
        Some(w as usize)
    } else {
        None
    }
}

/// Print a centered string in the terminsl padded by '='.
pub(crate) fn print_label(label: &'static str) {
    match get_term_width() {
        Some(width) => {
            let mut padding = width / 2 - 1 - label.len() / 2;
            let mut odd = (width % 2) == 1;
            if (label.len() % 2) == 1 {
                padding -= 1;
                odd = !odd;
            }
            println!(
                "\n{} {} {}",
                "=".repeat(padding),
                label,
                "=".repeat(padding + odd as usize),
            )
        }
        None => {
            println!("\n{}:", label)
        }
    }
}
