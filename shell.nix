{ nixpkgs ? import ./nix {}
, compiler ? "ghc884"
}:
let

  # Based on https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  chandra = import ./default.nix { inherit nixpkgs compiler; };

  hsPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      ghcide = self.callCabal2nix "ghcide-0.5.0" {};
    };
  };

  # extra = [ hsPkgs.ghcide hsPkgs.cabal-install
  extra = [ hsPkgs.cabal-install
            pkgs.heroku pkgs.postgresql pkgs.git ];
  buildInputs = chandra.env.nativeBuildInputs ++ extra;

in pkgs.stdenv.mkDerivation {
  name = "chandraobs-shell";
  buildInputs = buildInputs;

  # Hmm, not really building chandraobs, so presumably the phases
  # aren't getting triggered by 'nix-shell'. Should look at
  # https://github.com/maybevoid/maybevoid.com/blob/master/projects/2019-01-27-getting-started-haskell-nix/external.nix
  #
  postPhase = ''
    echo "***"
    echo "*** Welcome to chandraobservatory"
    echo "***"
  '';
}
