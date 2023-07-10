//! Import a signal file generated by the factorio mod.
// TODO: Add the mod name

use std::fs;
use std::path::PathBuf;

use directories::UserDirs;

const SIGNAL_FILE_NAME: &str = "chef.signals";

fn get_factorio_location() -> PathBuf {
    if cfg!(windows) {
        todo!("Factorio location for Windows not implemented yet")
    } else if cfg!(unix) {
        UserDirs::new().unwrap().home_dir().join(".factorio")
    } else {
        panic!("Unknown operating system.")
    }
}

pub fn import_signal_file(cwd: PathBuf) {
    let factorio_dir = get_factorio_location();
    let script_output_dir = factorio_dir.join("script-output");
    let signal_file = script_output_dir.join(SIGNAL_FILE_NAME);
    fs::copy(signal_file, cwd.join(SIGNAL_FILE_NAME)).expect("Could not copy file.");
}
