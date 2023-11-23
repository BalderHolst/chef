{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.05.tar.gz") { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    graphviz  # for creating visual graphs
    gnome.eog # svg viewer
  ];
}
