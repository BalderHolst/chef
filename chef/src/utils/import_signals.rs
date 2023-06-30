use std::path::{PathBuf, Path};
use std::fs;

use directories::UserDirs;

const signal_file_name: &str = "chef.signals";

fn get_factorio_location() -> PathBuf {
    if cfg!(windows) {
        todo!("Windows not implemented yet")
    } else if cfg!(unix) {
        UserDirs::new().unwrap().home_dir().join(".factorio")
    }
    else {
        panic!("Unknown operating system.")
    }
}

pub fn import_signal_file(cwd: PathBuf) {
    let factorio_dir = get_factorio_location();
    let script_output_dir = factorio_dir.join("script-output");
    let signal_file = script_output_dir.join(signal_file_name);
    fs::copy(signal_file, cwd.join(signal_file_name)).expect("Could not copy file.");
}
