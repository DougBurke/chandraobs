#!/bin/bash
#
# Arguments are <build-dir> <cache-dir> <env-dir>
#
# Clean up environment to remove un-needed files, and so reduce the
# slug size

set -e
set -o pipefail

BUILD_DIR="$1"

# All that should be needed is the webserver executable, libgmp
# and libffi, and the static files. It would be nice to clean out
# the vendor directory but leave that for now.
#
echo "-----> Cleaning up files in slug"
for dname in .cabal-sandbox .cabal ; do
    if [ -d $BUILD_DIR/$dname ]; then
        echo "-----> Deleting $dname"
        rm -f $BUILD_DIR/$dname
    fi
done

# Move the executable to somewhere a bit easier to find
APP="webserver"
if [ -f $BUILD_DIR/dist/build/$APP/$APP ]; then
    mkdir $BUILD_DIR/bin
    mv $BUILD_DIR/dist/build/$APP/$APP $BUILD_DIR/bin
else
    echo "ERROR: Unable to find $APP executable!"
    exit 1
fi
