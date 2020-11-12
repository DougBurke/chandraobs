{ nixpkgs ? import ./nix {}
, compiler ? "ghc884"
}:
let

  # Based on https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  chandra = import ./default.nix { inherit nixpkgs compiler; };

  # hsPkgs = pkgs.haskell.packages.${compiler}.override {
  #   overrides = self: super: {
  #     ghcide = self.callCabal2nix "ghcide-0.5.0" {};
  #   };
  # };

  # hsPkgs = pkgs.haskell.packages.${compiler}.override {
  #   overrides = self: super: {
  #     "implicit-hie" = self.callCabal2nix "implicit-hie-0.1.2.3" {};
  #   };
  # };

  # extra = [ hsPkgs.ghcide hsPkgs.cabal-install
  extra = [ haskellPackages.cabal-install
            # haskellPackages.haskell-language-server
	    # hsPkgs.cabal-install
	    # "hsPkgs.implicit-hie"
            pkgs.heroku pkgs.postgresql pkgs.git ];
  buildInputs = chandra.env.nativeBuildInputs ++ extra;

in pkgs.stdenv.mkDerivation {
  name = "chandraobs-shell";
  buildInputs = buildInputs;

  # The default LANG I use of en_US.UTF-8 doesn't seem to
  # work for some of the characters I see, so try the C
  # version.
  #
  shellHook = ''
    export LANG=C.UTF-8
    echo "***"
    echo "*** Welcome to chandraobservatory"
    echo "***"
  '';

}
