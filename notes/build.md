# Build notes

Heroku uses ghc 7.8.2, so should try to build locally with this.

% cabal clean
% cabal sandbox delete
% cabal sandbox init
% cabal install --only-dependencies --reorder-goals --dry-run
...

At the time of writing the only old package is

mtl-2.1.3.1 (latest: 2.2.0.1)

% cabal install --only-dependencies --reorder-goals
% cabal configure
% cabal run webserver

