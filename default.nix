{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chandraobs.nix { }
