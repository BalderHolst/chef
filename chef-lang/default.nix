{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-yZ+dz6n6iS4IzmNygYqe25anB3axtJ44U0BdTTBRpnY=";
  cargoDepsName = pname;
  doCheck = false;
}
