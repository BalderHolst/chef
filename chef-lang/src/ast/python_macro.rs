use std::{process::Command, rc::Rc};

use crate::{
    cli::Opts,
    diagnostics::{CompilationError, CompilationResult},
    text::{SourceText, TextSpan},
};

const DEFAULT_PYTHON: &str = "python3";

pub(crate) fn run_python_import(
    opts: Rc<Opts>,
    span: TextSpan,
    path: &str,
) -> CompilationResult<SourceText> {
    let python = opts.python.clone().unwrap_or(DEFAULT_PYTHON.to_string());

    if opts.verbose {
        println!("Running external command: `{python} {path}`")
    }

    let output = if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args([&python, &path.to_string()])
            .output()
    } else {
        Command::new("sh")
            .arg("-c")
            .arg(python + " " + path)
            .output()
    }
    .map_err(|e| {
        CompilationError::new_localized(
            format!("Failed to execute external macro command : {e}"),
            span,
        )
    })?;

    let code = String::from_utf8(output.stdout).unwrap();

    Ok(SourceText::from_str(code.as_str()))
}
