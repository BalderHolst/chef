//! The chef cli.

use std::{collections::HashMap, path::PathBuf};

use crate::blueprint::placement::PlacerName;
use clap::{Parser, Subcommand};
use termion::terminal_size;

fn parse_key_value(s: &str) -> Result<HashMap<String, String>, String> {
    let mut map = HashMap::new();
    for pair in s.split(":") {
        let (key, value) = pair
            .split_once("=")
            .ok_or(format!("Invalid key value pair: \"{}\"", pair))?;
        map.insert(key.to_string(), value.to_string());
    }
    Ok(map)
}

/// Command line options for chef
#[derive(Debug, Parser)]
pub struct Opts {
    /// Do not give cooking advice
    #[arg(short, long)]
    pub(crate) no_advice: bool,

    /// Be verbose
    #[arg(short, long)]
    pub(crate) verbose: bool,

    /// Python executable for macros
    #[arg(short('P'), long)]
    pub(crate) python: Option<String>,

    /// Directory to store temporary files
    // TODO: Crossplatform default
    #[arg(short('T'), long, default_value = "/tmp/chef")]
    pub(crate) tmp_dir: PathBuf,

    /// Include a modules at the given path
    #[arg(short('I'), long, value_parser = parse_key_value)]
    pub(crate) include: Option<HashMap<String, String>>,

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
            placer: PlacerName::default(),
            verbose: false,
            gui: false,
        });
        Self {
            no_advice: true,
            verbose: true,
            command,
            python: None,
            tmp_dir: PathBuf::from("/tmp/chef-test"),
            include: None,
        }
    }
}

/// Chef cli subcommands
#[derive(Debug, Subcommand)]
pub enum Command {
    /// Compile source code
    #[clap(aliases = &["c", "compile", "build"])]
    Cook(CookOpts),

    /// Simulate a chef program
    #[clap(alias = "sim")]
    Simulate(SimulateOpts),

    /// Add signals to your project
    Add(AddOpts),

    /// View a blueprint as json
    Inspect(InspectOpts),
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
    #[arg(short('g'), long)]
    pub(crate) graph: Option<String>,

    /// Output an svg to visualize the Factorio circuit connections.
    #[arg(short('G'), long)]
    pub(crate) fgraph: Option<String>,

    /// Choose the placering algorithm.
    #[arg(short('p'), long, default_value = "recursive")]
    pub placer: PlacerName,

    /// Show placement of combinators in gui
    #[arg(short('V'), long)]
    pub gui: bool,

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
            gui: false,
            placer: PlacerName::default(),
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

/// TODO:
#[derive(Debug, clap::Args)]
pub struct AddSignalOpts {}

/// Get the size of the current terminal that chef is running in.
fn get_term_width() -> Option<usize> {
    if let Ok((w, _)) = terminal_size() {
        Some(w as usize)
    } else {
        None
    }
}

#[derive(Debug, clap::Args)]
pub struct InspectOpts {
    pub blueprint: String,
}

/// Print a centered string in the terminal padded by '='.
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
