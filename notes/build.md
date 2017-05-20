# Build notes

I originally had documentation for the Heroku build in heroku.md, but
let's put them here as it should be simpler. Note that heroku.md does contain
other useful information (i.e. I have not really synthesised all the
information into a single document).

## Using Docker

### Using Stack locally

I plan to build a Docker image and push that to Heroku, as it avoids the
compile-time limit of the buildpack. To do this I am moving into the
twentieth century and using Stack https://docs.haskellstack.org/ (I could
use the new-build approach in cabal but there are advantages to having a
fixed set of packages as provided by Stack).

I am currently using Stack lts-8.14 which uses ghc 8.0.2, along with
version 1.4.0 of the Stack tooling:

    % stack setup
    % stack build --flag chandraobs:tools
    % stack exec getcurrent

### Building with Docker locally

The --build-arg option is needed to pass in the git revision number. The
name of the image is set to match that expected by Heroku.

    % sudo docker build -t registry.heroku.com/chandraobs-devel/web --build-arg SOURCE_VERSION=$(git rev-parse HEAD) .

### Testing out the Docker image

This will run the web server:

    % sudo docker run -it --network host registry.heroku.com/chandraobs-devel/web

To get access to the running container (e.g. to see if extra packages
can be removed):

    % sudo docker run -u 0 -it --network host registry.heroku.com/chandraobs-devel/web /bin/bash

### Using Docker to push to Heroku

From https://devcenter.heroku.com/articles/container-registry-and-runtime#pushing-an-image

    % docker tag <image> registry.heroku.com/<app>/<process-type>
    % docker push registry.heroku.com/<app>/<process-type>

means that, after building the image, we can push it directly with

    % sudo docker push registry.heroku.com/chandraobs-devel/web

We can not use the simple container:push option to heroku - i.e.

    % sudo heroku container:push web --remote heroku

since it does not support the --build-arg support; even if the image
has been built, calling this will just try to re-build it without the
SOURCE_VERSION setting.

## Using a Heroku buildpack

### Using cabal new-build

The current Haskell buildpack for heroku uses ghc 7.10.3, so if using
the buildpack approach (i.e. build on Heroku) then the local build should
also use this version. I originally used sandboxes (old steps below)
but have recently been using the "new-XXX" support in Cabal for "nix-style"
builds.

    % cabal update
    % cabal clean
    % cabal new-build -ftools

One problem with this is that there's no "easy" way to run an executable
using cabal; you have to find it's location in

    dist-newstyle/build/chandraobs-<version>/build/<exe>/<exe>

### Using cabal sandboxes

This is the orginal approach:

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


