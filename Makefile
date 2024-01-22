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

sym_install: build
	ln -s "$(CURDIR)/chef" "/usr/local/bin/chef"
	ln -sf "$(CURDIR)/chef-mod/chef_0.0.1/" "$(HOME)/.factorio/mods"
	pip install -e chef-python

install: build
	cp -v "./chef" "/usr/local/bin/chef"
	cp -rv "$(CURDIR)/chef-mod/chef_0.0.1/" "$(HOME)/.factorio/mods"
	pip install chef-python

clean:
	git clean -xdf
