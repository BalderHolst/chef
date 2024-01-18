cargo_flags = --manifest-path chef-lang/Cargo.toml

main: build

check: test lint

test:
	cargo test $(cargo_flags)
	./chef-python/test.py

lint:
	cargo fmt --check $(cargo_flags)
	cargo clippy $(cargo_flags)

build:
	cargo build --release $(cargo_flags)
	cp ./chef-lang/target/release/chef ./chef

install: build
	cp -v "./chef" "/usr/local/bin"
	pip install -e chef-python

clean:
	git clean -xdf
