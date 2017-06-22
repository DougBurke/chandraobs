#
# This is only for running commands. It is not intended to handle
# dependency resolution - e.g. to avoid a push or build if not needed.
#

APP=chandraobservatory
# APP=chandraobs-devel
# DBGAPP=chandraobs-devel-cedar-14

# HOMEDB=chandraobs
HOMEDB=chandraobs2

# How portable is this?
SOURCE_VERSION:=$(shell git rev-parse HEAD)

help:
	@echo "Targets are:"
	@echo "   help        - this page"
	@echo "   cleardb     - clear database on Heroku"
	@echo "   pushdb      - push database to Heroku"
	@echo "   builddocker - push docker images to Heroku"
	@echo "   rundocker   - run docker image"
	@echo "   pushdocker  - push docker images to Heroku"
	@echo "   stack       - stack build (incl. tools)"

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

pushdocker:
	@echo "### Pushing docker images to Heroku"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker push registry.heroku.com/${APP}/web

builddocker:
	@echo "### Making docker images"
	@echo "##"
	@echo "## ${APP}"
	@echo "##"
	@sudo docker build -t registry.heroku.com/${APP}/web --build-arg SOURCE_VERSION=${SOURCE_VERSION} .

rundocker:
	@echo "### Running ${APP} docker image locally"
	@echo "##"
	@sudo docker run -it --network host registry.heroku.com/${APP}/web

stack:
	@echo "### Building chandraobs via stack"
	@echo "##"
	@stack build --flag chandraobs:tools 
