#
# This is only for running commands. It is not intended to handle
# dependency resolution - e.g. to avoid a push or build if not needed.
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
	@echo ""
	@echo "   cleardb     - clear database on Heroku"
	@echo "   pushdb      - push database to Heroku"
	@echo ""
	@echo "   builddocker - build docker image"
	@echo "   rundocker   - run docker image"
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

builddocker:
	@echo "### Making docker image: webserver"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker build -t registry.heroku.com/${APP}/web --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

rundocker:
	@echo "### Running ${APP} docker image locally"
	@echo "##"
	@sudo docker run -it --network host registry.heroku.com/${APP}/web

pushdocker:
	@echo "### Pushing docker image to Heroku"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker push registry.heroku.com/${APP}/web

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
	@echo "## ${REDIRECT_APP}"
	@echo "##"
	@sudo docker push registry.heroku.com/${REDIRECT_APP}/web
