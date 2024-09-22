{
    description = "Nix flake for the Chef programming language";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self , nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
          let
              pkgs = nixpkgs.legacyPackages.${system};
              package = x: pkgs.callPackage x { inherit pkgs; };
          in
          {
            packages = {
                chef-lang      = package ./chef-lang;
                chef-inspector = package ./chef-inspector;
                chef-python    = package ./chef-python;
                chef-mod       = package ./chef-mod;
                default        = package ./.;
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

                    # Create dummy chef-inspector script that compiles it first if needed.
                    (pkgs.writeShellScriptBin "chef-inspector" ''
                        root="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                        cargo run --manifest-path "$root/chef-inspector/Cargo.toml" -- "$@"
                    '')

                ];

                LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [
                    wayland
                    libGL
                    libxkbcommon
                ]);

                shellHook = ''
                    # Determine project root
                    root="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"

                    # Add `chef` to to path
                    export PATH="$root/chef-lang/target/debug:$PATH"

                    # Add the chef python module to PYTHONPATH
                    export PYTHONPATH="$root/chef-python/src"
                '';
            };
        }
    );
}
