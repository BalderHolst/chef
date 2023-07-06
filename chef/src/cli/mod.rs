use gumdrop::Options;
use terminal_size::Width;

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

#[derive(Debug, Options)]
pub enum Command {
    #[options(help = "compile source code")]
    Cook(CookOpts),
    #[options(help = "add signals to your project")]
    Add(AddOpts),
}

#[derive(Debug, Options)]
pub struct CookOpts {
    #[options(help = "print help message")]
    pub(crate) help: bool,

    #[options(free)]
    pub(crate) files: Vec<String>
}

#[derive(Debug, Options)]
pub struct AddOpts {
    #[options(command)]
    pub(crate) command: Option<AddCommand>
}

#[derive(Debug, Options)]
pub enum AddCommand {
    #[options(help = "add signals exported from game with the factorio mod")]
    Signals(AddSignalOpts),
}

#[derive(Debug, Options)]
pub struct AddSignalOpts { }


fn get_term_width() -> Option<usize> {
    if let Some((Width(w), _)) = terminal_size::terminal_size() {
        Some(w as usize)
    }
    else {
        None
    }
}

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
