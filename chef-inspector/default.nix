{ pkgs ? import <nixpkgs> {} }:
let
    config = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = config.package.name;
  version = config.package.version;

  src = ./.;

  cargoHash = "sha256-cS15/CeQime2XLJHCIj/GxtdDiCxVMOO9+eQj9NVsDY=";
  cargoDepsName = pname;
  doCheck = false;
}
