{ compiler ? "ghc8107"
  , tools ? true
  , webserver ? true
}:

(import ./release.nix {
  compiler = compiler;
  tools = tools;
  webserver = webserver;
}).shell
