# How to buid with nix

I am trying out niv to make this repeatable.

% nix-shell -p niv --run "niv show"

% nix-shell -p niv --run "niv update nixpkgs -b nixos-20.03"

Then you should be able to

% nix-shell

and then develop with 'cabal build' (or 'new-build' if using an
older cabal-install).



