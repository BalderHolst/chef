{ pkgs, lib }:
let

    job-gen = import ./job-gen.nix { inherit pkgs lib; };

    root = "`git rev-parse --show-toplevel`";

    manifest-path = x: ''--manifest-path "${root}/${x}/Cargo.toml"'';

    compiler="chef-compiler";
    inspector="chef-inspector";
    crate="factorio-circuit-networks";

    cargo-fmt = x: /*bash*/ ''
        cargo fmt --check ${manifest-path x} \
            || { echo -e "\nPlease format your files in '${x}'.";  exit 1; }
    '';

    cargo-clippy = x: /*bash*/ ''
        cargo clippy ${manifest-path x} -- --deny warnings 2> /dev/null \
            || { echo -e "\nClippy is angry in '${x}'."; exit 1; }
    '';

    cargo-build = dir: bin: /*bash*/ ''
        cargo build --release ${manifest-path dir}
        mkdir -p ./bin
        cp ./${dir}/target/release/${bin} ./bin/${bin}
    '';

    cargo-test = x: /*bash*/ ''
        cargo test ${manifest-path x}
    '';

    install-bin = dir: bin: /*bash*/ ''
        cp -v ${root}/bin/${bin} /usr/local/bin/${bin}
    '';

    sym-install-bin = dir: bin: /*bash*/ ''
        ln -sv ${root}/bin/${bin} /usr/local/bin/${bin}
    '';

in
with job-gen;
rec {

    update-scripts = mkJob "update-scripts" { script = "nix run .#gen-scripts"; };
    check-git = mkJob "check-git" { script = ''
        git diff --quiet \
            || { echo -e "\nPlease commit your changes."; exit 1; }
    ''; };

    lint-fmt-compiler     = mkJob "lint-fmt-compiler"     { script = cargo-fmt    compiler;  };
    lint-fmt-inspector    = mkJob "lint-fmt-inspector"    { script = cargo-fmt    inspector; };
    lint-fmt-crate        = mkJob "lint-fmt-crate"        { script = cargo-fmt    crate;     };
    lint-clippy-compiler  = mkJob "lint-clippy-compiler"  { script = cargo-clippy compiler;  };
    lint-clippy-inspector = mkJob "lint-clippy-inspector" { script = cargo-clippy inspector; };
    lint-clippy-crate     = mkJob "lint-clippy-crate"     { script = cargo-clippy crate;     };

    lint-fmt    = jobSeq "lint-fmt"    [ lint-fmt-compiler    lint-fmt-inspector    lint-fmt-crate    ];
    lint-clippy = jobSeq "lint-clippy" [ lint-clippy-compiler lint-clippy-inspector lint-clippy-crate ];

    lint-compiler  = jobSeq "lint-compiler"  [ lint-fmt-compiler  lint-clippy-compiler  ];
    lint-inspector = jobSeq "lint-inspector" [ lint-fmt-inspector lint-clippy-inspector ];
    lint-crate     = jobSeq "lint-crate"     [ lint-fmt-crate     lint-clippy-crate     ];

    lint-all = jobSeq "lint-all" [ lint-fmt lint-clippy ];

    check-all = jobSeq "check-all" [
        check-git
        lint-all
    ];

    test-compiler  = mkJob "test-compiler"  { script = cargo-test compiler;  };
    test-inspector = mkJob "test-inspector" { script = cargo-test inspector; };
    test-crate     = mkJob "test-crate"     { script = cargo-test crate;     };

    test-all = jobSeq "test-all" [
        test-compiler
        test-inspector
    ];

    build-compiler  = mkJob "build-compiler"  { script = cargo-build compiler  "chef";           };
    build-inspector = mkJob "build-inspector" { script = cargo-build inspector "chef-inspector"; };
    build-crate     = mkJob "build-crate"     { script = cargo-build crate     "chef-crate";     };

    build-all = jobSeq "build-all" [
        build-compiler
        build-inspector
        build-crate
    ];

    install-compiler = mkJob "install-compiler" {
        script = install-bin compiler "chef";
        depends = [ build-compiler ];
    };
    install-inspector = mkJob "install-inspector" {
        script = install-bin inspector "chef-inspector";
        depends = [ build-inspector ];
    };
    install-python  = mkJob "install-python" {
        script = "pip install -e ${root}/chef-python";
    };

    install-all = jobSeq "install-all" [
        install-compiler
        install-inspector
        install-python
    ];

    sym-install-compiler = mkJob "sym-install-compiler" {
        script = sym-install-bin compiler "chef";
        depends = [ build-compiler ];
    };
    sym-install-inspector = mkJob "sym-install-inspector" {
        script = sym-install-bin inspector "chef-inspector";
        depends = [ build-inspector ];
    };
    sym-install-python  = mkJob "sym-install-python" {
        script = "pip sym-install -e ${root}/chef-python";
    };

    sym-install-all = jobSeq "sym-install-all" [
        sym-install-compiler
        sym-install-inspector
        sym-install-python
    ];
}
