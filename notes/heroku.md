
## Heroku

Set up for Heroku

    %  heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git

    % heroku apps
    === My Apps
    chandraobservatory
    peaceful-mountain-6182

This will then use the peaceful-mountain-6182 URL, which can be re-named:

    % heroku apps:rename chandraobs-devel
    Renaming peaceful-mountain-6182 to chandraobs-devel... done
    http://chandraobs-devel.herokuapp.com/ | git@heroku.com:chandraobs-devel.git
    Git remote heroku updated

so that the web server will be accessed via:

    http://chandraobs-devel.herokuapp.com/

Make sure that the Procfile is sensible; the example server looks
for the PORT environment variable so we can just have:

    % cat Procfile
    web: cabal run webserver

Information on the status and memory usage:

    % heroku apps:info
    === chandraobs-devel
    Git URL:       git@heroku.com:chandraobs-devel.git
    Owner Email:   dburke.gw@gmail.com
    Region:        us
    Repo Size:     252k
    Slug Size:     187M
    Stack:         cedar
    Web URL:       http://chandraobs-devel.herokuapp.com/

Push to Heroku

    % git push heroku master

Check Heroku

    % heroku ps
    % heroku logs
    % heroku logs -t
    % heroku releases
    % heroku status

Due to recent releases, a push to heroku ended up downgrading some
packages, which resulted in a broken build. So

  a) remove the current cash on heroku

  % heroku labs:enable buildpack-env-arg
   !    No such feature: buildpack-env-arg

  Hmmm. Let's try and set the environment variable anyway:

  % heroku config
  === chandraobs-devel Config Vars
  BUILDPACK_URL: https://github.com/begriffs/heroku-buildpack-ghc.git

  % heroku config:set CLEAR_CACHE=1
  Setting config vars and restarting chandraobs-devel... done, v38
  CLEAR_CACHE: 1
  % heroku config
  === chandraobs-devel Config Vars
  BUILDPACK_URL: https://github.com/begriffs/heroku-buildpack-ghc.git
  CLEAR_CACHE:   1

  Now, we could freeze the current versions using something like

    % cabal freeze
    Warning: The package list for 'hackage.haskell.org' is 16 days old.
    Run 'cabal update' to get the latest list of available packages.
    Resolving dependencies...
    % git add cabal.config
    ... git push

  but for now do not do this. The problem came because of a new version
  of Scotty (0.7.3 versus 0.7.2) which just bumps some constraints.

  Now do a 'git push heroku master' and you will see

    -----> Fetching custom git buildpack... done
    -----> Haskell app detected
    -----> Exporting config vars
           CLEAR_CACHE
    -----> Clearing the buildpack cache

  After the build, clear the variable with

  % heroku config:unset CLEAR_CACHE
  Unsetting CLEAR_CACHE and restarting chandraobs-devel... done, v39

  NOTE: from looking at the heroku output, it may be that the
  cabal installation of the dependencies does not use --reorder-goals,
  so it is possible that a different set of packages is being used.

