//! The chef cli.

use gumdrop::Options;
use terminal_size::Width;

/// Command line options for chef
#[derive(Debug, Options)]
pub struct Opts {
    #[options(help = "print help message")]
    pub(crate) help: bool,

    #[options(short = "q", help = "do not give cooking advice")]
    pub(crate) no_advice: bool,

    #[options(short = "v", help = "be verbose")]
    pub(crate) verbose: bool,

    #[options(command)]
    pub(crate) command: Option<Command>,
}

impl Default for Opts {
    fn default() -> Self {
        Self {
            help: false,
            no_advice: false,
            verbose: false,
            command: None,
        }
    }
}

/// Chef cli subcommands
#[derive(Debug, Options)]
pub enum Command {
    #[options(help = "compile source code")]
    Cook(CookOpts),
    #[options(help = "add signals to your project")]
    Add(AddOpts),
}

/// Options for the cli `cook` subcommand.
#[derive(Debug, Options)]
pub struct CookOpts {
    #[options(help = "print help message")]
    pub(crate) help: bool,

    #[options(free)]
    pub(crate) files: Vec<String>
}

/// Options for the cli `add` subcommand.
#[derive(Debug, Options)]
pub struct AddOpts {
    #[options(command)]
    pub(crate) command: Option<AddCommand>
}

/// The cli `add` subcommand, used for adding signal files to the project.
#[derive(Debug, Options)]
pub enum AddCommand {
    #[options(help = "add signals exported from game with the factorio mod")]
    Signals(AddSignalOpts),
}

/// TODO
#[derive(Debug, Options)]
pub struct AddSignalOpts { }


/// Get the size of the current terminal that chef is running in.
fn get_term_width() -> Option<usize> {
    if let Some((Width(w), _)) = terminal_size::terminal_size() {
        Some(w as usize)
    }
    else {
        None
    }
}

/// Print a centered string in the terminsl padded by '='.
pub(crate) fn print_label(label: &'static str) {
    match get_term_width() {
        Some(width) => {
            let mut padding = width / 2 - 1 - label.len()/2;
            let mut odd = (width % 2) == 1;
            if (label.len() % 2) == 1 {
                padding -= 1;
                odd = !odd;
            }
            println!("\n{} {} {}", 
                     "=".repeat(padding),
                     label,
                     "=".repeat(padding + odd as usize),
                     )
        },
        None => {
            println!("\n{}:", label)
        },
    }
}
