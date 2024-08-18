{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-3OzvKVcUDP1YqnExDvnf0kwiibk1K1Y4iNtZdD+hQ/M=";
  cargoDepsName = pname;
  doCheck = false;
}
