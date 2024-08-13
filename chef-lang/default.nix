{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-c9lbJpfnhOtuZb4SXfLtlKKHH7yI12q37Hlj6VRLKW8=";
  cargoDepsName = pname;
  doCheck = false;
}
