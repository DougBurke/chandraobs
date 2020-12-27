{ nixpkgs ? import ./nix {}
, compiler ? "ghc8102" }:

let
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
    groundhog = self.callCabal2nix "groundhog-0.11.0" {};
    groundhog-th = self.callCabal2nix "groundhog-th-0.11" {};
    groundhog-postgresql = self.callCabal2nix "groundhog-postgresql-0.11" {};
    };
  };
in
  myHaskellPackages.callCabal2nix "chandraobs" (./.) {}