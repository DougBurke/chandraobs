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
# DBGAPP=chandraobs-devel-cedar-14

# HOMEDB=chandraobs
HOMEDB=chandraobs2

# How portable is this?
SOURCE_VERSION:=$(shell git rev-parse HEAD)

help:
	@echo "Targets are:"
	@echo "   help        - this page"
	@echo ""
	@echo "   stack       - stack build (incl. tools)"
	@echo "   showstack   - show the stack build command (incl. tools)"
	@echo ""
	@echo "   cleardb     - clear database on Heroku"
	@echo "   pushdb      - push database to Heroku"
	@echo ""
	@echo "   showdocker  - docker command to build the image"
	@echo "   builddocker - build docker image"
	@echo "   cleandocker - build docker image (with no cache)"
	@echo "   rundocker   - run docker image (local)"
	@echo ""
	@echo "   runheroku   - build/run Heroku docker image loclly"
	@echo "   pushdocker  - push docker image to Heroku"
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

cleardb:
	@echo "### Clearing database on Heroku"
	@echo "##"
	@echo "## To ${APP}"
	@echo "##"
	-@heroku pg:reset DATABASE_URL --confirm ${APP} --app ${APP}

pushdb:	cleardb
	@echo "### Pushing database to Heroku"
	@echo "##"
	@echo "## To ${APP}"
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

rundocker:
	@echo "### Running ${APP} docker image locally"
	@echo "##"
	@sudo docker run -it --network host registry.heroku.com/${APP}/web

runheroku:
	@echo "### Build and run Heroku docker image locally"
	@echo "##"
	@echo "## [will also push it to Heroku but not release it]"
	@echo "## [not sure I understand the Heroku docker integration..]"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo heroku container:push web --app ${APP} --arg SOURCE_VERSION=${SOURCE_VERSION}
	@sudo heroku container:run web --app ${APP}

pushdocker:
	@echo "### Pushing docker image to Heroku"
	@echo "##"
	@echo "## [will also build it]"
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
