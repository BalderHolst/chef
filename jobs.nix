{ pkgs, lib, root }:
let
    job-gen = import ./job-gen.nix { inherit pkgs lib root; };

    manifest-path = x: ''--manifest-path "${root}/${x}/Cargo.toml"'';

    compiler="chef-compiler";
    inspector="chef-inspector";
    check-fmt = x: /*bash*/ ''
        echo "Checking formatting for ${x}..."
        cargo fmt --check ${manifest-path x} || {
            echo -e "\nPlease format your files in '${x}'."
                exit 1
        }
    '';

    check-clippy = x: /*bash*/ ''
        echo "Linting ${x}..."
        cargo clippy ${manifest-path x} -- --deny warnings 2> /dev/null || {
            echo -e "\nClippy is angry in '${x}'."
            exit 1
        }
    '';

    build-bin = dir: bin: /*bash*/ ''
        echo "Building ${bin} in ${dir}..."
        cargo build --release ${manifest-path dir}
        mkdir -p ./bin
        cp ./${dir}/target/release/${bin} ./bin/${bin}
    '';

    test-bin = x: /*bash*/ ''
        echo "Testing ${x}..."
        cargo test ${manifest-path x}
    '';

    install-bin = dir: bin: /*bash*/ ''
        echo "Installing ${bin}..."
        # Build the binary
        ${build-bin dir bin}
        ln -s ${root}/bin/${bin} /usr/local/bin/${bin}
    '';

in
with job-gen;
rec {
    check-fmt-compiler     = mkJob "check-fmt-compiler"     { script = check-fmt    compiler;  };
    check-fmt-inspector    = mkJob "check-fmt-inspector"    { script = check-fmt    inspector; };
    check-clippy-compiler  = mkJob "check-clippy-compiler"  { script = check-clippy compiler;  };
    check-clippy-inspector = mkJob "check-clippy-inspector" { script = check-clippy inspector; };

    check-all = jobSeq "check-all" [
        check-fmt-compiler
        check-fmt-inspector
        check-clippy-compiler
        check-clippy-inspector
    ];

    test-compiler  = mkJob "test-compiler"  { script = test-bin compiler;  };
    test-inspector = mkJob "test-inspector" { script = test-bin inspector; };

    test-all = jobSeq "test-all" [
        test-compiler
        test-inspector
    ];

    build-compiler  = mkJob "build-compiler"  { script = build-bin compiler  "chef";           };
    build-inspector = mkJob "build-inspector" { script = build-bin inspector "chef-inspector"; };

    build-all = jobSeq "build-all" [
        build-compiler
        build-inspector
    ];

    install-compiler  = mkJob "install-compiler"  { script = install-bin compiler "chef";            };
    install-inspector = mkJob "install-inspector" { script = install-bin inspector "chef-inspector"; };
    install-python    = mkJob "install-python"    { script = "pip install -e ${root}/chef-python";   };

    install-all = jobSeq "install-all" [
        install-compiler
        install-inspector
        install-python
    ];
}
