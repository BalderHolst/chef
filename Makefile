cargo_flags = --manifest-path chef/Cargo.toml

main: test

test:
	cargo fmt --check $(cargo_flags) || exit 1
	cargo clippy $(cargo_flags) || exit 1
	cargo test $(cargo_flags) || exit 1
