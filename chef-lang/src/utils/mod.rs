//! Utilities for chef.

use std::{
    env,
    io::{self, Write},
};

use graphviz_rust::{self, cmd::Format, printer::PrinterContext};

pub mod import_signals;

pub const BASE_SIGNALS: &str = include_str!("base.signals");

pub type VisualizerResult = Result<String, VisualizerError>;

#[derive(Debug)]
pub enum VisualizerError {
    IoErr(io::Error),
    GraphvizIoError(io::Error),
    GraphvizError(String),
}

pub fn save_to_clipboard(s: &str) {
    #[allow(clippy::if_same_then_else)] // TODO: remove this
    if cfg!(target_os = "windows") {
        // TODO: Add clipboard support for MacOS
    } else if cfg!(target_os = "macos") {
        // TODO: Add clipboard support for MacOS
    } else if cfg!(target_os = "linux") {
        // TODO: Test x11 support
        if env::var("XDG_SESSION_TYPE") == Ok("x11".to_string()) {
            if let Ok(mut xclip) = std::process::Command::new("xclip")
                .arg("-selection")
                .arg("clipboard")
                .stdin(std::process::Stdio::piped())
                .spawn()
            {
                let _ = xclip.stdin.as_mut().unwrap().write_all(s.as_bytes());
            }
        }
        // Otherwise assume Wayland
        else {
            const EXE: &str = "wl-copy";
            if let Ok(mut wl_copy) = std::process::Command::new(EXE)
                .stdin(std::process::Stdio::piped())
                .spawn()
            {
                let _ = wl_copy.stdin.as_mut().unwrap().write_all(s.as_bytes());
            }
        }
    }
}

pub fn dot_to_svg(dot: String) -> VisualizerResult {
    let dot_graph = match graphviz_rust::parse(dot.as_str()) {
        Ok(g) => g,
        Err(e) => return Err(VisualizerError::GraphvizError(e)),
    };

    match graphviz_rust::exec(
        dot_graph,
        &mut PrinterContext::default(),
        vec![Format::Svg.into()],
    ) {
        Ok(s) => Ok(String::from_utf8(s).unwrap()),
        Err(e) => Err(VisualizerError::GraphvizIoError(e)),
    }
}
