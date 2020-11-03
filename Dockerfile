# This is based on the Dockerfiles from
#   https://arow.info/blog/posts/2017-03-30-servant-on-heroku.html
#   http://mashina.io/psto/docker-haskell
# but all mistakes are mine
#

FROM heroku/heroku:18 AS build

ENV LANG C.UTF-8

# Set up needed code for using Stack and the installation
#
RUN apt-get update \
  && apt-get remove -y --assume-yes imagemagick imagemagick-6-common \
  && apt-get upgrade -y --assume-yes \
  && apt-get install -y --assume-yes \
  g++ \
  gcc \
  libc6-dev \
  libffi-dev \
  libgmp-dev \
  make \
  xz-utils \
  zlib1g-dev \
  git \
  gnupg \
  libpq-dev

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

RUN mkdir -p /opt/chandraobs/src
RUN mkdir -p /opt/chandraobs/bin
WORKDIR /opt/chandraobs/src

# Copy over files in stages to avoid unnescessary rebuilds; the assumption
# is that stack.yaml changes less than the cabal file, which is less than
# the code itself. It seems that changes to stack have made this less
# obvious a "win".
#
ENV PATH "$PATH:/opt/stack/bin:/opt/chandraobs/bin"

COPY ./stack.yaml /opt/chandraobs/src/stack.yaml
COPY ./chandraobs.cabal /opt/chandraobs/src/chandraobs.cabal
RUN stack --no-terminal setup

# Install dependencies and check they are okay
ENV FLAGS --flag chandraobs:server --flag chandraobs:-redirectserver --flag chandraobs:-tools

RUN stack --no-terminal test --only-dependencies ${FLAGS}

# Use this to pass in the GIT commit id; it is named this to match the
# heroku slug set up.
ARG SOURCE_VERSION
ENV SOURCE_VERSION ${SOURCE_VERSION}

# Only copy over what we need
COPY autogen/ /opt/chandraobs/src/autogen/
COPY ./Setup.hs /opt/chandraobs/src
COPY ./configure /opt/chandraobs/src
COPY lib/ /opt/chandraobs/src/lib/
COPY app/ /opt/chandraobs/src/app/

RUN stack --no-terminal build ${FLAGS}
RUN stack --no-terminal --local-bin-path /opt/chandraobs/bin install ${FLAGS}

# Report the ghc and stack versions (after the build so it gets reported
# each time)
RUN stack exec ghc -- --version
RUN stack --version

# Copy over the static code (do this after the build so that simple changes
# don't trigger a re-build).
#
COPY static/ /opt/chandraobs/static/

FROM heroku/heroku:18 AS deploy

ENV LANG C.UTF-8

# Copy over the files we want
COPY --from=build /opt/chandraobs/bin/ /opt/chandraobs/bin/
COPY --from=build /opt/chandraobs/static/ /opt/chandraobs/static/

# Occasionally I review the output of 'apt list' and add or remove
# packages. Not exactly robust
#
# This used to include imagemagick but that caused some problems
# (an update lead to confusion because the configuration file had been
#  updated, which meant that the update failed), so I have removed it
# above.
#
#  imagemagick \
#
RUN apt-get remove -y --assume-yes \
  ghostscript \
  geoip-database \
  ruby \
  rake \
  gcc \
  make \
  xz-utils \
  git \
  telnet \
  ed \
  wget \
  curl \
  unzip \
  zip \
  bzip2 \
  perl \
  python2.7 \
  python3.6 \
  mysql-common \
  openssh-client \
  openssh-server \
  gsfonts \
  fonts-dejavu-core \
  rsync \
  mtools \
  libxcb1 \
  libxml2 \
  && apt-get autoremove -y --assume-yes \
  && apt-get purge -y --assume-yes \
  && apt-get clean \
  && apt-get autoclean

# Ensure what we do have left is up to date.
#
RUN apt-get update \
  && apt-get upgrade -y --assume-yes

# Remove apt caches to reduce the size of our container.
#
RUN rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash webserver
RUN chown -R webserver:webserver /opt/chandraobs
USER webserver
# ENV PATH "$PATH:/opt/chandraobs/bin"

WORKDIR /opt/chandraobs

CMD /opt/chandraobs/bin/webserver
