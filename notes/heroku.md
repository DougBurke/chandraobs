
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
packages, which resulted in a broken build. So I want to
remove the current build cash on heroku:

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
cabal installation of the dependencies does not use `--reorder-goals`,
so it is possible that a different set of packages is being used. I guess
the buildpack does not use this `cabal` flag to speed up compilation time.

How to change the buildpack in use - e.g. to use a bug fix that hasn't
been accepted yet, such as
[this bug](https://github.com/begriffs/heroku-buildpack-ghc/pull/61):

    % heroku config | grep BUILDPACK
    BUILDPACK_URL:              https://github.com/begriffs/heroku-buildpack-ghc.git
    % heroku config:set BUILDPACK_URL=https://github.com/kantp/heroku-buildpack-ghc.git

and then the build works (hopefully), which can be reset later by saying

    % heroku config:set BUILDPACK_URL=https://github.com/begriffs/heroku-buildpack-ghc.git

### A crash

I managed to crash the server due to a database migration that could not
be done withou manual intervention. This lead to

    % heroku ps
    === web (1X): `cabal run webserver`
    web.1: crashed 2014/06/11 20:10:09 (~ 5m ago)

and this could be re-started (once the database had been clean up) by
saying

    % heroku ps:restart web.1
    Restarting web.1 dyno... done

## Upgrading from cedar-10 to cedar-14

% git remote -v
heroku	https://git.heroku.com/chandraobs-devel.git (fetch)
heroku	https://git.heroku.com/chandraobs-devel.git (push)
origin	https://bitbucket.org/doug_burke/chandraobs.git (fetch)
origin	https://bitbucket.org/doug_burke/chandraobs.git (push)
% git branch --list --all
* master
  remotes/origin/HEAD -> origin/master
  remotes/origin/api
  remotes/origin/groundhog
  remotes/origin/master

Following https://devcenter.heroku.com/articles/cedar-14-migration

% heroku create --remote heroku-cedar-14 --stack cedar-14 chandraobs-devel-cedar-14
Creating chandraobs-devel-cedar-14... done, stack is cedar-14
https://chandraobs-devel-cedar-14.herokuapp.com/ | https://git.heroku.com/chandraobs-devel-cedar-14.git

% heroku apps
=== My Apps
chandraobs-devel
chandraobs-devel-cedar-14
chandraobservatory

%  heroku apps:info
 ▸    Error: Multiple apps in git remotes
 ▸    Usage: heroku apps:info --remote heroku-cedar-14
 ▸       or: heroku apps:info --app chandraobs-devel-cedar-14
 ▸    
 ▸    Your local git repository has more than 1 app referenced in git remotes.
 ▸    Because of this, we can't determine which app you want to run this command against.
 ▸    Specify the app you want with --app or --remote.
 ▸    
 ▸    Heroku remotes in repo:
 ▸    heroku          (chandraobs-devel)
 ▸    heroku-cedar-14 (chandraobs-devel-cedar-14)
 ▸    
 ▸    https://devcenter.heroku.com/articles/multiple-environments

% heroku apps:info --remote heroku
=== chandraobs-devel
Addons:        heroku-postgresql:hobby-dev
Dynos:         web: 1
Git URL:       https://git.heroku.com/chandraobs-devel.git
Owner:         dburke.gw@gmail.com
Region:        us
Repo Size:     9 MB
Slug Size:     253 MB
Stack:         cedar
Web URL:       https://chandraobs-devel.herokuapp.com/
% heroku apps:info --remote heroku-cedar-14
=== chandraobs-devel-cedar-14
Dynos:         
Git URL:       https://git.heroku.com/chandraobs-devel-cedar-14.git
Owner:         dburke.gw@gmail.com
Region:        us
Repo Size:     0 B
Slug Size:     0 B
Stack:         cedar-14
Web URL:       https://chandraobs-devel-cedar-14.herokuapp.com/

% heroku config --remote heroku | grep BUILDPACK
BUILDPACK_URL:              https://github.com/begriffs/heroku-buildpack-ghc.git
% heroku config --remote heroku-cedar-14 | grep BUILDPACK
%

Now, checking begriffs buildpack, it looks to have been updated for cedar-14,
but points out that https://github.com/mietek/haskell-on-heroku is likely a faster
build (due to its use of pre-built binaries). Leave that for a later day.

% heroku config:set --remote heroku-cedar-14 BUILDPACK_URL=https://github.com/begriffs/heroku-buildpack-ghc.git
Setting config vars and restarting chandraobs-devel-cedar-14... done
BUILDPACK_URL: https://github.com/begriffs/heroku-buildpack-ghc.git

Need to add postgres: 

% heroku addons --remote heroku

Add-on                                    Plan       Price
────────────────────────────────────────  ─────────  ─────
heroku-postgresql (sailing-quietly-1739)  hobby-dev  free
 └─ as HEROKU_POSTGRESQL_ROSE

The table above shows add-ons and the attachments to the current app (chandraobs-devel) or other apps.

% heroku addons --remote heroku-cedar-14
No add-ons for app chandraobs-devel-cedar-14.

% heroku addons:add heroku-postgresql --remote heroku-cedar-14
ARNING: `heroku addons:add` has been deprecated. Please use `heroku addons:create` instead.
Creating postgresql-dimensional-49291... done, (free)
Adding postgresql-dimensional-49291 to chandraobs-devel-cedar-14... done
Setting DATABASE_URL and restarting chandraobs-devel-cedar-14... done, v4
Database has been created and is available
 ! This database is empty. If upgrading, you can transfer
 ! data from another database with pg:copy
Use `heroku addons:docs heroku-postgresql` to view documentation.

% heroku pg:info --remote heroku
=== HEROKU_POSTGRESQL_ROSE_URL, DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.3.9
Created:     2014-06-06 17:40 UTC
Data Size:   9.4 MB
Tables:      8
Rows:        8161/10000 (In compliance, close to row limit)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      sailing-quietly-1739

% heroku pg:info --remote heroku-cedar-14
=== DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.4.4
Created:     2016-02-24 13:36 UTC
Data Size:   6.5 MB
Tables:      0
Rows:        0/10000 (In compliance)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      postgresql-dimensional-49291

I could not work out what the database names to use in 'heroku pg:copy' were
(too lazy to check), so just push the data from the laptop.

% heroku pg:reset DATABASE_URL --confirm chandraobs-devel-cedar-14

Just check it hasn't clobbered the "main" database:

% heroku pg:info --remote heroku
=== HEROKU_POSTGRESQL_ROSE_URL, DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.3.9
Created:     2014-06-06 17:40 UTC
Data Size:   9.4 MB
Tables:      8
Rows:        8161/10000 (In compliance, close to row limit)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      sailing-quietly-1739

% PGUSER=postgres PGPASSWORD=postgres heroku pg:push chandraobs DATABASE_URL --app chandraobs-devel-cedar-14
...

% heroku pg:info --remote heroku-cedar-14
=== DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.4.4
Created:     2016-02-24 13:36 UTC
Data Size:   8.5 MB
Tables:      8
Rows:        8161/10000 (In compliance)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      postgresql-dimensional-49291

Now try and build the web server:

% heroku pg:info --remote heroku-cedar-14
=== DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.4.4
Created:     2016-02-24 13:36 UTC
Data Size:   8.5 MB
Tables:      8
Rows:        8161/10000 (In compliance)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      postgresql-dimensional-49291

% heroku apps:info --remote heroku-cedar-14
=== chandraobs-devel-cedar-14
Addons:        heroku-postgresql:hobby-dev
Dynos:         
Git URL:       https://git.heroku.com/chandraobs-devel-cedar-14.git
Owner:         dburke.gw@gmail.com
Region:        us
Repo Size:     0 B
Slug Size:     0 B
Stack:         cedar-14
Web URL:       https://chandraobs-devel-cedar-14.herokuapp.com/

% git push heroku-cedar-14 master
Counting objects: 1658, done.
Delta compression using up to 8 threads.
Compressing objects: 100% (601/601), done.
Writing objects: 100% (1658/1658), 1.25 MiB | 708.00 KiB/s, done.
Total 1658 (delta 1078), reused 1578 (delta 1026)
remote: Compressing source files... done.
remote: Building source:
remote: 
remote: -----> Fetching set buildpack https://github.com/begriffs/heroku-buildpack-ghc.git... done
remote: -----> Haskell app detected
remote: -----> Exporting config vars
remote:        DATABASE_URL
remote: cat: /tmp/d20160224-158-rxvvvy/PATH: No such file or directory
remote: -----> Linking libncurses.so in an accessible place
remote: -----> Installing prebuilt libffi-3.0.13 into cache
remote: ######################################################################## 100.0%
remote: -----> Restoring libffi files from cache
remote: -----> Installing prebuilt libpcre-8.35 into cache
remote: ######################################################################## 100.0%
remote: -----> Restoring libpcre files from cache
remote: -----> Installing prebuilt libgmp-6.0.0a into cache
remote: ######################################################################## 100.0%
remote: -----> Restoring libgmp files from cache
remote: -----> Installing prebuilt GHC 7.8.3
...
remote: Package has never been configured. Configuring with default flags. If this
remote: fails, please run configure manually.
remote: Resolving dependencies...
remote: Configuring chandraobs-0.0.5...
remote: Building chandraobs-0.0.5...
remote: Preprocessing library chandraobs-0.0.5...
remote: [1 of 2] Compiling Types            ( lib/Types.hs, dist/build/Types.o )
remote: Loading package ghc-prim ... linking ... done.
remote: Loading package integer-gmp ... linking ... done.
remote: Loading package base ... linking ... done.
remote: Loading package array-0.5.0.0 ... linking ... done.
remote: Loading package deepseq-1.3.0.2 ... linking ... done.
remote: Loading package bytestring-0.10.4.0 ... linking ... done.
remote: Loading package containers-0.5.5.1 ... linking ... done.
remote: Loading package binary-0.7.1.0 ... linking ... done.
remote: Loading package text-1.2.2.0 ... linking ... done.
remote: Loading package hashable-1.2.4.0 ... linking ... done.
remote: Loading package transformers-0.4.3.0 ... linking ... done.
remote: Loading package primitive-0.6.1.0 ... linking ... done.
remote: Loading package vector-0.11.0.0 ... linking ... done.
remote: Loading package scientific-0.3.4.4 ... linking ... done.
remote: Loading package attoparsec-0.13.0.1 ... linking ... done.
remote: Loading package dlist-0.7.1.2 ... linking ... done.
remote: Loading package mtl-2.2.1 ... linking ... done.
remote: Loading package old-locale-1.0.0.6 ... linking ... done.
remote: Loading package syb-0.6 ... linking ... done.
remote: Loading package pretty-1.1.1.1 ... linking ... done.
remote: Loading package template-haskell ... linking ... done.
remote: Loading package time-1.4.2 ... linking ... done.
remote: Loading package unordered-containers-0.2.7.0 ... linking ... done.
remote: Loading package aeson-0.9.0.1 ... linking ... done.
remote: Loading package blaze-builder-0.4.0.1 ... linking ... done.
remote: Loading package case-insensitive-1.2.0.5 ... linking ... done.
remote: Loading package data-default-class-0.0.1 ... linking ... done.
remote: Loading package http-types-0.9 ... linking ... done.
remote: Loading package stm-2.4.4.1 ... linking ... done.
remote: Loading package transformers-compat-0.4.0.4 ... linking ... done.
remote: Loading package transformers-base-0.4.4 ... linking ... done.
remote: Loading package monad-control-1.0.0.5 ... linking ... done.
remote: Loading package nats-1.1 ... linking ... done.
remote: Loading package unix-2.7.0.1 ... linking ... done.
remote: Loading package network-2.6.2.1 ... linking ... done.
remote: Loading package regex-base-0.93.2 ... linking ... done.
remote: Loading package regex-posix-0.95.2 ... linking ... done.
remote: Loading package regex-compat-0.95.1 ... linking ... done.
remote: Loading package bytestring-builder-0.10.6.0.0 ... linking ... done.
remote: Loading package vault-0.3.0.6 ... linking ... done.
remote: Loading package wai-3.2.0 ... linking ... done.
remote: Loading package ansi-terminal-0.6.2.3 ... linking ... done.
remote: Loading package base64-bytestring-1.0.0.1 ... linking ... done.
remote: Loading package cookie-0.4.1.6 ... linking ... done.
remote: Loading package filepath-1.3.0.2 ... linking ... done.
remote: Loading package directory-1.2.1.0 ... linking ... done.
remote: Loading package auto-update-0.1.3 ... linking ... done.
remote: Loading package fast-logger-2.4.1 ... linking ... done.
remote: Loading package appar-0.1.4 ... linking ... done.
remote: Loading package byteorder-1.0.4 ... linking ... done.
remote: Loading package iproute-1.7.0 ... linking ... done.
remote: Loading package lifted-base-0.2.3.6 ... linking ... done.
remote: Loading package exceptions-0.8.2.1 ... linking ... done.
remote: Loading package mmorph-1.0.6 ... linking ... done.
remote: Loading package resourcet-1.1.7.2 ... linking ... done.
remote: Loading package async-2.1.0 ... linking ... done.
remote: Loading package process-1.2.0.0 ... linking ... done.
remote: Loading package random-1.1 ... linking ... done.
remote: Loading package zlib-0.6.1.1 ... linking ... done.
remote: Loading package streaming-commons-0.1.15.1 ... linking ... done.
remote: Loading package stringsearch-0.3.6.6 ... linking ... done.
remote: Loading package unix-compat-0.4.1.4 ... linking ... done.
remote: Loading package tagged-0.8.3 ... linking ... done.
remote: Loading package semigroups-0.18.1 ... linking ... done.
remote: Loading package void-0.7.1 ... linking ... done.
remote: Loading package easy-file-0.2.1 ... linking ... done.
remote: Loading package old-time-1.1.0.2 ... linking ... done.
remote: Loading package unix-time-0.3.6 ... linking ... done.
remote: Loading package wai-logger-2.2.5 ... linking ... done.
remote: Loading package word8-0.1.2 ... linking ... done.
remote: Loading package wai-extra-3.0.14 ... linking ... done.
remote: Loading package http-date-0.0.6.1 ... linking ... done.
remote: Loading package psqueues-0.2.2.0 ... linking ... done.
remote: Loading package http2-1.5.1 ... linking ... done.
remote: Loading package simple-sendfile-0.2.21 ... linking ... done.
remote: Loading package warp-3.2.3 ... linking ... done.
remote: Loading package scotty-0.10.2 ... linking ... done.
remote: Loading package conduit-1.2.6.2 ... linking ... done.
remote: Loading package conduit-extra-1.1.10.1 ... linking ... done.
remote: Loading package monad-loops-0.4.3 ... linking ... done.
remote: Loading package stm-chans-3.0.0.4 ... linking ... done.
remote: Loading package monad-logger-0.3.18 ... linking ... done.
remote: Loading package groundhog-0.7.0.3 ... linking ... done.
remote: Loading package enclosed-exceptions-1.0.1.1 ... linking ... done.
remote: Loading package yaml-0.8.16 ... linking ... done.
remote: Loading package groundhog-th-0.7.0.1 ... linking ... done.
remote: Loading package postgresql-libpq-0.9.1.1 ... linking ... done.
remote: Loading package uuid-types-1.0.2 ... linking ... done.
remote: Loading package postgresql-simple-0.5.1.2 ... linking ... done.
remote: Loading package resource-pool-0.2.3.2 ... linking ... done.
remote: Loading package groundhog-postgresql-0.7.0.2 ... linking ... done.
remote: Loading package blaze-markup-0.7.0.3 ... linking ... done.
remote: Loading package blaze-html-0.8.1.1 ... linking ... done.
remote: [2 of 2] Compiling Database         ( lib/Database.hs, dist/build/Database.o )
remote: In-place registering chandraobs-0.0.5...
remote: Preprocessing executable 'webserver' for chandraobs-0.0.5...
remote: [ 1 of 13] Compiling Utils            ( app/Utils.hs, dist/build/webserver/webserver-tmp/Utils.o )
remote: [ 2 of 13] Compiling Views.Render     ( app/Views/Render.hs, dist/build/webserver/webserver-tmp/Views/Render.o )
remote: [ 3 of 13] Compiling Views.Record     ( app/Views/Record.hs, dist/build/webserver/webserver-tmp/Views/Record.o )
remote: [ 4 of 13] Compiling Views.Search.Category ( app/Views/Search/Category.hs, dist/build/webserver/webserver-tmp/Views/Search/Category.o )
remote: [ 5 of 13] Compiling Views.Search.Constellation ( app/Views/Search/Constellation.hs, dist/build/webserver/webserver-tmp/Views/Search/Constellation.o )
remote: [ 6 of 13] Compiling Views.Search.Instrument ( app/Views/Search/Instrument.hs, dist/build/webserver/webserver-tmp/Views/Search/Instrument.o )
remote: [ 7 of 13] Compiling Views.Search.Types ( app/Views/Search/Types.hs, dist/build/webserver/webserver-tmp/Views/Search/Types.o )
remote: [ 8 of 13] Compiling Views.Schedule   ( app/Views/Schedule.hs, dist/build/webserver/webserver-tmp/Views/Schedule.o )
remote: [ 9 of 13] Compiling Views.WWT        ( app/Views/WWT.hs, dist/build/webserver/webserver-tmp/Views/WWT.o )
remote: [10 of 13] Compiling Views.Proposal   ( app/Views/Proposal.hs, dist/build/webserver/webserver-tmp/Views/Proposal.o )
remote: [11 of 13] Compiling Views.NotFound   ( app/Views/NotFound.hs, dist/build/webserver/webserver-tmp/Views/NotFound.o )
remote: [12 of 13] Compiling Views.Index      ( app/Views/Index.hs, dist/build/webserver/webserver-tmp/Views/Index.o )
remote: [13 of 13] Compiling Main             ( app/WebServer.hs, dist/build/webserver/webserver-tmp/Main.o )
remote: Linking dist/build/webserver/webserver ...
remote: -----> Cleaning any source downloads from the build cache
remote: -----> Caching latest cabal sandbox
remote: -----> Making GHC binaries available to Heroku command line
remote: -----> Making cabal config available to Heroku command line
remote: -----> Making cabal sandbox available to Heroku command line
remote: 
remote: -----> Discovering process types
remote:        Procfile declares types -> web
remote: 
remote: -----> Compressing...
remote:        Done: 250.7M
remote: -----> Launching...
remote:        Released v5
remote:        https://chandraobs-devel-cedar-14.herokuapp.com/ deployed to Heroku
remote: 
remote: Verifying deploy... done.
To https://git.heroku.com/chandraobs-devel-cedar-14.git
 * [new branch]      master -> master

Have eventually got things working (using https resources for JS libs), so
switch the (not-really) production branch to cedar-14:

% heroku stack:set cedar-14 -a chandraobs-devel
Stack set. Next release on chandraobs-devel will use cedar-14.
Run git push heroku master to create a new release on chandraobs-devel.

Needed to clear the cache for the rebuild to work.

# Slug too large?

With the move to ghc 7.10 (or perhaps it is extra packages being picked
up) the slug size exceeds 300Mb. I have hacked up the buildpack to allow
a script to be run to delete some unneeded files (more could probably
be done), and this means that - for now - the buildpack URL needs changing
and an environment variable set up:

% heroku buildpacks --app chandraobs-devel
=== chandraobs-devel Buildpack URL
https://github.com/begriffs/heroku-buildpack-ghc.git
% heroku buildpacks:remove --app chandraobs-devel https://github.com/begriffs/heroku-buildpack-ghc.git
Buildpack removed.
 ▸    The BUILDPACK_URL config var is still set and will be used for the next
 ▸    release
% heroku config:unset --app chandraobs-devel BUILDPACK_URL
Unsetting BUILDPACK_URL and restarting ⬢ chandraobs-devel... done, v192
% heroku buildpacks:add --app chandraobs-devel https://DougBurke@github.com/DougBurke/heroku-buildpack-ghc#add-post-script
Buildpack added. Next release on chandraobs-devel will use https://DougBurke@github.com/DougBurke/heroku-buildpack-ghc#add-post-script.
Run git push heroku master to create a new release using this buildpack.
heroku config:set POST_SCRIPT=heroku-compile-post-script.sh --app chandraobs-devel
Setting POST_SCRIPT and restarting ⬢ chandraobs-devel... done, v193
POST_SCRIPT: heroku-compile-post-script.sh

Can now push the repository (which should contain the script
heroky-compile-post-script.sh)

% git push heroku master
