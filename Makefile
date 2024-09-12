chef_lang = --manifest-path chef-lang/Cargo.toml
chef_inspector = --manifest-path chef-inspector/Cargo.toml

main: build

check: test lint

test:
	@make --silent test-lang
	@make --silent test-python

test-lang:
	@echo "============================== Running Language Tests =============================="
	cargo test $(chef_lang)

test-python:
	@echo "=========================== Running Python Package Tests ==========================="
	./chef-python/test.py

lint:
	cargo fmt --check $(chef_lang)
	cargo fmt --check $(chef_inspector)
	cargo clippy $(chef_lang)
	cargo clippy $(chef_inspector)

build:
	cargo build --release $(chef_lang)
	cargo build --release $(chef_inspector)
	mkdir -p ./bin
	cp ./chef-lang/target/release/chef ./bin/chef
	cp ./chef-inspector/target/release/chef-inspector ./bin/chef-inspector

sym_install: build
	ln -s "$(CURDIR)/chef" "/usr/local/bin/chef"
	ln -s "$(CURDIR)/chef-inspector" "/usr/local/bin/chef-inspector"
	ln -sf "$(CURDIR)/chef-mod/chef_0.0.1/" "$(HOME)/.factorio/mods"
	pip install -e chef-python

install: build
	cp -v "./chef" "/usr/local/bin/chef"
	cp -v "./chef-inspector" "/usr/local/bin/chef-inspector"
	cp -rv "$(CURDIR)/chef-mod/chef_0.0.1/" "$(HOME)/.factorio/mods"
	pip install chef-python

clean:
	git clean -xdf
