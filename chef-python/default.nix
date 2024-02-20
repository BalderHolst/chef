{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz") { } }:

pkgs.python310Packages.buildPythonPackage rec {
  pname = "chef";
  version = "0.0.1";
  src = ./.;
  doCheck = false;
  propagatedBuildInputs = [ ];
}
