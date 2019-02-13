#
# This is only for running commands. It is not intended to handle
# dependency resolution - e.g. to avoid a push or build if not needed.
#
# There have been changes to how heroku/docker integration works; not
# entirely convinced that the following has been fully updated to
# handle this.
#

APP=chandraobservatory
REDIRECT_APP=chandraobs-devel

# Support for using a local postgres instance is currently off the table.
#
# HOMEDB=chandraobs
# HOMEDB=chandraobs2

PORT=3000
DATABASE_URL:=$(shell heroku config:get --app ${APP} DATABASE_URL)

# How portable is this?
SOURCE_VERSION:=$(shell git rev-parse HEAD)

help:
	@echo "Targets are:"
	@echo "   help        - this page"
	@echo ""
	@echo "   stack       - stack build (incl. tools)"
	@echo "   showstack   - show the stack build command (incl. tools)"
	@echo ""
	@echo "   cabcurrent  - cabal run getcurrent"
	@echo "   cabupdate   - cabal run updateschedule"
	@echo "   cabget      - cabal run updateschedule"
	@echo "   cabweb      - cabal run webserver"
	@echo ""
	@echo "   cleardb     - clear database on Heroku"
	@echo "   pushdb      - push database to Heroku (currently unused)"
	@echo ""
	@echo "   showdocker  - docker command to build the image"
	@echo "   builddocker - build docker image (webserver)"
	@echo "   cleandocker - build docker image (with no cache)"
	@echo "   rundocker   - run docker image (webserver; heroku db)"
	@echo ""
	@echo "   buildtools  - build docker image (tools; needs CIAO tools)"
	@echo "   runtools    - run docker image (tools; heroku db)"
	@echo ""
	@echo "   pushdocker  - push docker image to Heroku (webserver)"
	@echo ""
	@echo "   buildredirect - build docker image for redirect"
	@echo "   runredirect   - run docker image for redirect"
	@echo "   pushdredirect - push docker image for redirect"
	@echo ""

stack:
	@echo "### Building chandraobs via stack"
	@echo "##"
	@echo "## Note: redirectserver is not built"
	@echo "##"
	@stack build --flag chandraobs:tools --flag chandraobs:server --flag chandraobs:-redirectserver

showstack:
	@echo "### Build chandraobs via stack"
	@echo "##"
	@echo "## Note: redirectserver is not built"
	@echo "##"
	@echo "stack build --flag chandraobs:tools --flag chandraobs:server --flag chandraobs:-redirectserver"

cabcurrent:
	@echo "### run getcurrent on production db via cabal"
	@echo "##"
	@echo "## APP=${APP}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@DATABASE_URL=${DATABASE_URL} cabal v2-run getcurrent

cabupdate:
	@echo "### run updateschedule on production db via cabal"
	@echo "##"
	@echo "## APP=${APP}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@DATABASE_URL=${DATABASE_URL} cabal v2-run updateschedule

cabget:
	@echo "### run getschedule on production db via cabal"
	@echo "##"
	@echo "## APP=${APP}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@DATABASE_URL=${DATABASE_URL} cabal v2-run getschedule

cabweb:
	@echo "### run webserver on production db via cabal"
	@echo "##"
	@echo "## APP=${APP}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@DATABASE_URL=${DATABASE_URL} cabal v2-run webserver

cleardb:
	@echo "### Clearing database on Heroku"
	@echo "##"
	@echo "## To ${APP}"
	@echo "##"
	-@heroku pg:reset DATABASE_URL --confirm ${APP} --app ${APP}

pushdb:	cleardb
	@echo "ERROR - pushdb is currently unused"; exit 1
	@echo "### Pushing database to Heroku"
	@echo "##"
	@echo "## To ${APP} From ${HOMEDB}"
	@echo "##"
	-@PGUSER=postgres PGPASSWORD=postgres PGHOST=127.0.0.1 heroku pg:push ${HOMEDB} DATABASE_URL --app ${APP}

showdocker:
	@echo "### What is the command to make the docker image: webserver"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@echo sudo docker build -t registry.heroku.com/${APP}/web --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

builddocker:
	@echo "### Making docker image: webserver"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker build -t registry.heroku.com/${APP}/web --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

cleandocker:
	@echo "### Making docker image: webserver (no cache)"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker build --no-cache -t registry.heroku.com/${APP}/web --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

buildtools:
	@echo "### Making docker image: tools (needs CIAO tools)"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker build -t ${APP}.tools --file Dockerfile.tools --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

rundocker:
	@echo "### Running ${APP} docker image locally"
	@echo "##"
	@echo "## ${APP}"
	@echo "## PORT=${PORT}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@echo "##"
	@sudo docker run -it --network host --env PORT=${PORT} --env DATABASE_URL=${DATABASE_URL} registry.heroku.com/${APP}/web

runtools:
	@echo "### Running docker image: tools"
	@echo "##"
	@echo "## ${APP}"
	@echo "## DATABASE_URL=${DATABASE_URL}"
	@echo "##"
	@sudo docker run -it --env DATABASE_URL=${DATABASE_URL} ${APP}.tools /bin/bash

pushdocker:
	@echo "### Pushing docker image to Heroku"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo heroku container:push web --app ${APP} --arg SOURCE_VERSION=${SOURCE_VERSION}
	@sudo heroku container:release web --app ${APP}

buildredirect:
	@echo "### Making docker image: redirect"
	@echo "##"
	@echo "## ${REDIRECT_APP}"
	@echo "##"
	@sudo docker build -t registry.heroku.com/${REDIRECT_APP}/web --file=Dockerfile.redirect .

runredirect:
	@echo "### Running ${REDIRECT_APP} docker image locally"
	@echo "##"
	@sudo docker run -it --network host registry.heroku.com/${REDIRECT_APP}/web

pushredirect:
	@echo "### Pushing redirect docker imageto Heroku"
	@echo "##"
	@echo "## [will also build it]"
	@echo "##"
	@echo "## ${REDIRECT_APP}"
	@echo "##"
	@echo "## NOTE: this is currently broken, as does not"
	@echo "##       use Dockerfile.redirect"
	@echo "##"
	@not-a-command sudo heroku container:push web --app ${REDIRECT_APP}
	@not-a-command sudo heroku container:release web --app ${REDIRECT_APP}
