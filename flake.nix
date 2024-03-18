{
    description = "Nix Flake for sqlite-integrated.";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    };

    outputs = { self , nixpkgs ,... }: let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
    in
    {

        packages."${system}" = rec {
            chef-lang = pkgs.callPackage ./chef-lang {};
            chef-python = pkgs.callPackage ./chef-python {};
            chef-mod = ./chef-mod;
            default = pkgs.stdenv.mkDerivation {
                name = "chef";
                unpackPhase = "true"; # no source
                installPhase = ''
                    mkdir $out -p
                    cp -rv ${chef-lang}/* $out
                    cp -rv ${chef-python}/* $out
                    mkdir $out/mod -p
                    cp -rv ${chef-mod}/* $out/mod
                    '';
            };
        };

        # Run compiler
        apps."${system}".default = {
            type = "app";
            program = "${pkgs.callPackage ./chef-lang {} }/bin/chef";
        };

        # Development shell
        devShells."${system}" = {
            default = pkgs.mkShell {
                packages = [
                    pkgs.cargo     # compiler and build system
                    pkgs.graphviz  # for creating visual graphs
                    pkgs.gnome.eog # svg viewer
                ];

                shellHook = ''
                    root="$(pwd)"
                    [[ ! "$(basename $root)" = "chef" ]] && echo -e "\nWARNING: Paths may are not set correctly. Please run in the 'chef' root directory."
                    export PATH="$root/chef-lang/target/debug:$PATH"
                    export PYTHONPATH="$root/chef-python/src"
                '';
            };
            ci = pkgs.mkShell {
                packages = [ pkgs.cargo ];

                shellHook = ''
                    [[ -z "$GITHUB_WORKSPACE" ]] && {
                        echo -e "\nWARNING: GITHUB_WORKSPACE is not set. Please run in a GitHub Actions environment."
                        GITHUB_WORKSPACE=$(pwd)
                        echo "Using '$GITHUB_WORKSPACE' instead."
                    }
                    root="$GITHUB_WORKSPACE"
                    [[ ! "$(basename $root)" = "chef" ]] && echo -e "\nWARNING: Paths may are not set correctly. Please run in the 'chef' root directory."
                    export PATH="$root/chef-lang/target/debug:$PATH"
                    export PYTHONPATH="$root/chef-python/src"
                '';
            };
        };
    };
}
