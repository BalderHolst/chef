use factorio_blueprint::{BlueprintCodec, Container};

pub mod graph;

pub fn decode_blueprint_string(string: &str) {
    if let Container::Blueprint(b) = BlueprintCodec::decode_string(string).unwrap() {

    }

}
