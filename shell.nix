{ nixpkgs ? import ./nix {}
# , compiler ? "ghc8102"
, compiler ? "ghc884"
, support ? false
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

  opt = if support then [ haskellPackages.haskell-language-server ] else [];
  extra = [ haskellPackages.cabal-install haskellPackages.hlint
            pkgs.heroku pkgs.postgresql pkgs.git pkgs.niv ]
	  ++ opt;
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
