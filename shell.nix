{ pkgs ? import <nixpkgs> {}}:
  pkgs.mkShell {
      nativeBuildInputs = [ pkgs.buildPackages.binutils];
  }
