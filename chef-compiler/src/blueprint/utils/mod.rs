use factorio_blueprint as fb;
use jsonxf;

#[derive(Debug)]
enum BlueprintInspectError {
    InvalidBlueprintString,
    Other(fb::Error),
}

impl std::error::Error for BlueprintInspectError {}

impl std::fmt::Display for BlueprintInspectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidBlueprintString => f.write_str("Invalid blueprint string"),
            Self::Other(other) => other.fmt(f),
        }
    }
}

/// Decode a Factorio blueprint string into the underlying JSON string.
pub fn blueprint_to_json(blueprint_str: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut buf = Vec::new();
    fb::BlueprintCodec::decode_reader(blueprint_str.as_bytes(), |mut reader| {
        jsonxf::pretty_print_stream(&mut reader, &mut buf)
    })
    .map_err(|e| match e {
        // TODO: Invalid blueprint error is an IO error for some reason. Find a way
        // to detect this to not assume that all IO errors are invalid blueprint strings.
        fb::Error::Io(_io_error) => BlueprintInspectError::InvalidBlueprintString,
        e => BlueprintInspectError::Other(e),
    })?;
    Ok(String::from_utf8(buf)?)
}
