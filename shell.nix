{ nixpkgs ? import ./nix {}
, compiler ? "ghc883"
}:
let

  # Based on https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  chandra = import ./default.nix { inherit nixpkgs compiler; };

  extra = [ haskellPackages.cabal-install pkgs.heroku pkgs.git ];

in pkgs.stdenv.mkDerivation {
  name = "chandraobs-shell";
  buildInputs = chandra.env.nativeBuildInputs ++ extra;

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
