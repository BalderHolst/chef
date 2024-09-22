{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-Auq2OHDepnuQVURH//fuaV1pnyhHtNUCQBIwJhgsmc8=";
  cargoDepsName = pname;
  doCheck = false;
}
