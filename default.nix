{ pkgs ? import <nixpkgs> {} }:
let
    chef-compiler = pkgs.callPackage ./chef-compiler { inherit pkgs; };
    chef-mod = pkgs.callPackage ./chef-mod { inherit pkgs; };
    chef-python = pkgs.callPackage ./chef-python { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
    name = "chef";
    unpackPhase = "true"; # no source
    installPhase = ''
        mkdir $out -p
        cp -rv ${chef-compiler}/* $out
        cp -rv ${chef-python}/* $out
        mkdir $out/mod -p
        cp -rv ${chef-mod}/* $out/mod
    '';
}

