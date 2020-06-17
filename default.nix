{ nixpkgs ? import ./nix {}
, compiler ? "ghc883" }:

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chandraobs.nix { }
