//! This module contains the logic for running python scripts as macros in the chef language.

use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use crate::{
    cli::Opts,
    diagnostics::{CompilationError, CompilationResult},
    error,
    text::{SourceText, TextSpan},
};

/// Find an executable in the system's PATH
// TODO: Test on windows
fn find_executable<P>(exe_name: P) -> Option<PathBuf>
where
    P: AsRef<Path>,
{
    env::var_os("PATH").and_then(|paths| {
        env::split_paths(&paths).find_map(|dir| {
            let full_path = dir.join(&exe_name);
            if full_path.is_file() {
                Some(full_path)
            } else {
                None
            }
        })
    })
}

/// Find the python executable in the system's PATH
fn find_python() -> Option<PathBuf> {
    find_executable("python3").or(find_executable("python"))
}

/// Run a python script and return the output as a chef source code
pub(crate) fn run_python_import(
    opts: Rc<Opts>,
    span: Option<TextSpan>,
    path: &str,
    name: Option<String>,
    inputs: Option<String>,
) -> CompilationResult<SourceText> {
    let python = match &opts.python {
        Some(default_python) => default_python.clone(),
        None => match find_python() {
            Some(system_python) => system_python.into_os_string().into_string().unwrap(),
            None => return Err(error!("Could not locate python executable in $PATH. You can supply one with the `--python` flag.")),
        },
    };

    let name = match name {
        Some(name) => name,
        None => "".to_string(),
    };

    let inputs = match inputs {
        Some(inputs) => inputs,
        None => "".to_string(),
    };

    let output = if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args([&python, &path.to_string(), &name, &inputs])
            .output()
    } else {
        let inputs = inputs.replace('\"', "\\\"");

        let name = format!("\"{}\"", name);
        let inputs = format!("\"{}\"", inputs);

        let cmd = python + " " + path + " " + &name + " " + &inputs;

        if opts.verbose {
            println!("Running external command: `{cmd}`")
        }

        Command::new("sh").arg("-c").arg(cmd).output()
    }
    .map_err(|e| {
        let message = format!("Failed to execute external macro command : {e}");
        match &span {
            Some(span) => CompilationError::new_localized(message, span.clone()),
            None => CompilationError::new_generic(message),
        }
    })?;

    if !output.status.success() {
        let message = format!(
            "Failed running python script '{}':\n{}",
            path,
            String::from_utf8(output.stderr).unwrap()
        );
        return Err(match &span {
            Some(span) => CompilationError::new_localized(message, span.clone()),
            None => CompilationError::new_generic(message),
        });
    };

    let code = String::from_utf8(output.stdout).unwrap();

    let tmp_dir = opts.tmp_dir.join("macros");
    let _ = fs::create_dir_all(&tmp_dir);

    let path = tmp_dir
        .join(format!("block-{}.rcp", name))
        .display()
        .to_string();

    Ok(SourceText::new(path, code))
}
