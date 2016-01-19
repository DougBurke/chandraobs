# Build notes

Heroku uses ghc 7.8.2, so should try to build locally with this.

    % cabal clean
    % cabal sandbox delete
    % cabal sandbox init
    % cabal install --only-dependencies -ftools --reorder-goals --dry-run

To build and run just the webserver:

    % cabal install --only-dependencies --reorder-goals
    % cabal configure
    % cabal run webserver

To build everything:

    % cabal install --only-dependencies -ftools --reorder-goals
    % cabal configure -ftools
    % cabal build


