{ pkgs, root }:
let
    compiler="chef-compiler";
    inspector="chef-inspector";

    manifest-path = x: ''--manifest-path "${root}/${x}/Cargo.toml"'';

    script-msg = ''
        # This script was generated by Nix.
        # To make changes edit the `jobs.nix` file.

    '';

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
rec {

    combine = jobs: builtins.concatStringsSep "\n" jobs;
    make-script = name: script-jobs: pkgs.writeShellScript name ''
        ${ script-msg }
        ${ combine script-jobs }
    '';

    jobs = rec {

        check-fmt-compiler = check-fmt compiler;
        check-fmt-inspector = check-fmt inspector;
        check-clippy-compiler = check-clippy compiler;
        check-clippy-inspector = check-clippy inspector;

        check-all = combine [
            check-fmt-compiler
            check-fmt-inspector
            check-clippy-compiler
            check-clippy-inspector
        ];

        test-compiler = test-bin compiler;
        test-inspector = test-bin inspector;

        test-all = combine [
            test-compiler
            test-inspector
        ];

        build-compiler = build-bin compiler "chef";
        build-inspector = build-bin inspector "chef-inspector";

        build-all = combine [
            build-compiler
            build-inspector
        ];

        install-compiler = install-bin compiler "chef";
        install-inspector = install-bin inspector "chef-inspector";
        install-python = /*bash*/ "pip install -e ${root}/chef-python";

        install-all = combine [
            install-compiler
            install-inspector
            install-python
        ];
    };
}
