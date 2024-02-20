{ pkgs ? import <nixpkgs> {} }:
pkgs.rustPlatform.buildRustPackage rec {
  pname = "chef";
  version = "0.1.0";

  src = ./.;

  cargoHash = "sha256-rz3hJk3GvR2x1QiCiT9IY6aIescjbtggqIq3czT3x6w=";
  cargoDepsName = pname;
  doCheck = false;
}
