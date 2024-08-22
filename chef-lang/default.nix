{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-KmVoTs8IKdSPhAeT8eo28lteJYFylKBJKzkJFUwI2Yg=";
  cargoDepsName = pname;
  doCheck = false;
}
