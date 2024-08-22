extern crate static_files_proc;
pub use static_files_proc::*;

pub enum StaticFile {
    Directory {
        name: &'static str,
        files: &'static [StaticFile],
    },
    File {
        name: &'static str,
        contents: &'static str,
    },
}
