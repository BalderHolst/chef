{
    description = "Nix flake for the Chef programming language";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self , nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
          let pkgs = nixpkgs.legacyPackages.${system}; in
          rec {
            packages = rec {
                chef-lang = pkgs.callPackage ./chef-lang { inherit pkgs; };
                chef-inspector = pkgs.callPackage ./chef-inspector { inherit pkgs; };
                chef-python = pkgs.callPackage ./chef-python { inherit pkgs; };
                chef-mod = pkgs.callPackage ./chef-mod { inherit pkgs; };
                default = pkgs.stdenv.mkDerivation {
                    name = "chef";
                    unpackPhase = "true"; # no source
                    installPhase = ''
                        mkdir $out -p
                        cp -rv ${chef-lang}/* $out
                        cp -rv ${chef-inspector}/* $out
                        cp -rv ${chef-python}/* $out
                        mkdir $out/mod -p
                        cp -rv ${chef-mod}/* $out/mod
                        '';
                };
            };
            apps = rec {
                compiler = flake-utils.lib.mkApp { drv = self.packages.${system}.chef-lang; };
                inspector = flake-utils.lib.mkApp { drv = self.packages.${system}.chef-inspector; };
                default = compiler;
            };

            # Development shell
            devShells.default = pkgs.mkShell {
                buildInputs = with pkgs; [
                    cargo         # compiler and build system
                    rust-analyzer # lsp for rust
                    graphviz      # for creating visual graphs
                    gnome.eog     # svg viewer
                ];

                LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [
                    wayland
                    libGL
                    libxkbcommon
                ]);

                shellHook = ''
                    root="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                    [[ ! "$(basename $root)" = "chef" ]] && echo -e "\nWARNING: Paths may are not set correctly. Please run in the 'chef' root directory."
                    export PATH="$root/chef-lang/target/debug:$PATH"
                    export PYTHONPATH="$root/chef-python/src"
                '';
            };
        }
    );
}
