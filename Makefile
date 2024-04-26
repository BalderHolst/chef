cargo_flags = --manifest-path chef-lang/Cargo.toml

main: build

check: test lint

test:
	@echo "============================== Running Language Tests =============================="
	@make --silent test-lang
	@echo "=========================== Running Python Package Tests ==========================="
	@make --silent test-python

test-lang:
	cargo test $(cargo_flags)

test-python:
	./chef-python/test.py

lint-fmt:
	cargo fmt --check $(cargo_flags)

lint-clippy:
	cargo clippy $(cargo_flags)

lint:
	@make --silent lint-fmt
	@make --silent lint-clippy

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
