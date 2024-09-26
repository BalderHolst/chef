{ pkgs ? import <nixpkgs> {} }:
let
    chef-compiler = pkgs.callPackage ./chef-compiler { inherit pkgs; };
    chef-mod = pkgs.callPackage ./chef-mod { inherit pkgs; };
    chef-python = pkgs.callPackage ./chef-python { inherit pkgs; };
    chef-inspector = pkgs.callPackage ./chef-inspector { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
    name = "chef";
    unpackPhase = "true"; # no source
    installPhase = ''
        mkdir $out/bin -p
        cp -rv ${chef-compiler}/bin/* $out/bin
        cp -rv ${chef-inspector}/bin/* $out/bin
        cp -rv ${chef-python}/* $out
        mkdir $out/mod -p
        cp -rv ${chef-mod}/* $out/mod
    '';
}

