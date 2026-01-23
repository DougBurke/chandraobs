{ compiler ? "ghc9103"
  , tools ? false
  , webserver ? false
}:

(import ./release.nix {
  compiler = compiler;
  tools = tools;
  webserver = webserver;
}).shell
