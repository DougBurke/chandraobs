#!/bin/sh
#
# Insert the git commit value into the files from autogen/.
# One problem with this approach is that the build system is
# unaware of these relationships, so will not pick up changes to
# the autogen/ files without manual intervention (e.g. manually
# running `sh add-commit.sh`).
#
# This used to be used within the cabal configuration setup, and
# so could be run from different locations. Now we require this
# before the cabal build step, so it's easier.
#
echo `pwd`
infile=autogen/lib-Git.hs.in
# outfile=autogen/Git.hs
outfile=lib/Git.hs

if [ ! -f $infile ] ; then
  echo "*** ERROR: $infile not found (input)";
  exit 1;
fi

if [ -f $outfile ] ; then
  echo "*** ERROR: $outfile already exists (output)";
  exit 1;
fi

# On heroku, using the buildpack/slug, the git setup has already been
# removed by the time this is run, but fortunately it is available as the
# SOURCE_VERSION environment variable - see
# https://devcenter.heroku.com/changelog-items/630
#
# When building under docker we ensure the git revision is sent in
# using the same environment variable.
#
# To support building with nix, as I don't know how to pass the
# environment variable through to the cabal call, I am now adding
# in "reading from a file" (but in that case we could just
# avoid this whole configure step anyway ...).
#
if [ -z "$SOURCE_VERSION" ]; then
    commit=$(git rev-parse HEAD)
else
    commit="$SOURCE_VERSION"
fi

echo "Using id: $commit"

# Replace the gitCommitId line with an actual setting..
#
if [ -z "$commit" ]; then
    # Ideally this should not happen
    cp $infile $outfile
else
    sed -e 's/gitCommitId = Nothing/gitCommitId = Just (CId (pack "'$commit'"))/' $infile > $outfile
fi

# end
