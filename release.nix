# See https://github.com/utdemir/hs-nix-template
#
{ compiler ? "ghc8107" }:

let
  isDefaultCompiler = compiler == "ghc8107";
  
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      # "groundhog" = hself.hackage2nix "groundhog" "0.11.0";
      # "groundhog-th" = hself.hackage2nix "groundhog-th" "0.11";
      # "groundhog-postgresql" = hself.hackage2nix "groundhog-postgresql" "0.11";

      # "chandraobs" = hself.callCabal2nix "chandraobs" (gitignore ./.) {};
      "chandraobs" = hself.callCabal2nixWithOptions "chandraobs" (gitignore ./.) "-ftools" {};
    };
  };

  # How do I implement the following?
  #  
  # # The default LANG I use of en_US.UTF-8 doesn't seem to
  # # work for some of the characters I see, so try the C
  # # version.
  # #
  # shellHook = ''
  #   export LANG=C.UTF-8
  #   echo "***"
  #   echo "*** Welcome to chandraobservatory"
  #   echo "***"
  # '';

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."chandraobs"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.heroku
      pkgs.postgresql
      pkgs.git
    ] ++ pkgs.lib.optionals isDefaultCompiler [
      # pkgs.haskellPackages.haskell-language-server
      myHaskellPackages.haskell-language-server
    ];
    withHoogle = isDefaultCompiler;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."chandraobs");

  # docker = pkgs.dockerTools.buildImage {
  #   name = "{{cookiecutter.project_name}}";
  #   config.Cmd = [ "${exe}/bin/{{cookiecutter.project_name}}" ];
  # };

in
{
  inherit shell;
  inherit exe;
  # inherit docker;
  inherit myHaskellPackages;
  "chandraobs" = myHaskellPackages."chandraobs";
}
