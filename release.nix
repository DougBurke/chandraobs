# See https://github.com/utdemir/hs-nix-template
#
{ compiler ? "ghc8107"
  , tools ? true
  , webserver ? true
}:

let
  isDefaultCompiler = compiler == "ghc8107";

  # too lazy to work out the nix expression language
  flags = if tools && webserver then
          "--flag=tools"
	  else if tools then
	    "--flag='tools -webserver'"
	    else if webserver then
	      ""
	      else "--flag=-webserver";

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      # "groundhog" = hself.hackage2nix "groundhog" "0.11.0";
      # "groundhog-th" = hself.hackage2nix "groundhog-th" "0.11";
      # "groundhog-postgresql" = hself.hackage2nix "groundhog-postgresql" "0.11";

      "chandraobs" = hself.callCabal2nixWithOptions "chandraobs" (gitignore ./.) flags {};
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

  # hack around from
  # https://jeancharles.quillet.org/posts/2022-04-22-Embed-the-git-hash-into-a-binary-with-nix.html
  #
  drv = myHaskellPackages."chandraobs";
  drv2 = pkgs.haskell.lib.addBuildTool drv pkgs.git;
  exe = pkgs.haskell.lib.justStaticExecutables drv2;

  # this only makes sense if webserver is set but do not check this.
  #
  docker = pkgs.dockerTools.buildImage {
    name = "chandraobservatory";

    # How do we provide access to the static/ directory?
    # For some reason using copyToRoot causes buildImage to
    # fail; I wonder if we are using too-old a version of nixpkgs?
    #
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      pathsToLink = [ "${drv}/static" ];
    };

    config.Cmd = [ "${exe}/bin/webserver" ];
  };

in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "chandraobs" = myHaskellPackages."chandraobs";
}
