extern crate static_files_proc;
pub use static_files_proc::*;

#[derive(Debug)]
pub enum Error {
    NotFound,
}

type Result<T> = std::result::Result<T, Error>;

pub struct FileStash {
    pub files: &'static [File],
}

impl FileStash {
    pub fn get_file(&'static self, path: &str) -> Result<&'static File> {
        self.files.iter().find(|f| f.path == path).ok_or(Error::NotFound)
    }
}

pub struct File {
    pub path: &'static str,
    pub contents: &'static str,
}
