use clap::Parser;
use clap_stdin::MaybeStdin;

#[derive(Debug, Parser)]
pub struct Opts {
    #[clap(default_value = "-")]
    pub blueprint: MaybeStdin<String>,
}
