{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
    name = "chef-mod";
    src = ./.;
    buildPhase = "";
    installPhase = ''
        mkdir -p $out
        cp -rv * $out
    '';
}
