{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoDepsName = pname;
  cargoHash = "";

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "factorio-blueprint-0.3.0" = "sha256-zc8ffweDdMYSiDbjOAhfUBWYObD0GtY9nN8nlq/B4bU=";
    };
  };

  doCheck = false;
}
