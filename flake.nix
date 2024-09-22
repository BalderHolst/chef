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
                chef-compiler  = package ./chef-compiler;
                chef-inspector = package ./chef-inspector;
                chef-python    = package ./chef-python;
                chef-mod       = package ./chef-mod;
                default        = package ./.;
            };
            apps = rec {
                compiler = flake-utils.lib.mkApp { drv = self.packages.${system}.chef-compiler; };
                inspector = flake-utils.lib.mkApp { drv = self.packages.${system}.chef-inspector; };
                default = compiler;
            };

            # Development shell
            devShells.default =
            let
                debug-bin = crate: exe: pkgs.writeShellScriptBin exe ''
                    root="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                    if [ ! -f "$root/${crate}/target/debug/${exe}" ]; then
                        cargo build --manifest-path "$root/${crate}/Cargo.toml"
                    fi
                    cargo run --quiet --manifest-path "$root/${crate}/Cargo.toml" -- $@
                '';
            in
            pkgs.mkShell {
                buildInputs = with pkgs; [
                    git           # version control
                    cargo         # compiler and build system
                    rust-analyzer # lsp for rust
                    graphviz      # for creating visual graphs
                    gnome.eog     # svg viewer

                    # Add project exeutables to path (build them if they don't exist)
                    (debug-bin "chef-compiler" "chef")
                    (debug-bin "chef-inspector" "chef-inspector")

                ];

                LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [
                    wayland
                    libGL
                    libxkbcommon
                ]);

                shellHook = ''
                    # Determine project root
                    root="$(git rev-parse --show-toplevel)"

                    # Add `chef` to to path
                    export PATH="$root/chef-compiler/target/debug:$PATH"

                    # Add the chef python module to PYTHONPATH
                    export PYTHONPATH="$root/chef-python/src"

                    # Set up git hooks
                    git config set core.hooksPath ".hooks"
                '';
            };
        }
    );
}
