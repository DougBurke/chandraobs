# This is based on the Dockerfiles from
#   https://arow.info/blog/posts/2017-03-30-servant-on-heroku.html
#   http://mashina.io/psto/docker-haskell
# but all mistakes are mine
#

FROM heroku/heroku:16

# Use this to pass in the GIT commit id; it is named this to match the
# heroku slug set up.
ARG SOURCE_VERSION

ENV LANG C.UTF-8

# Remove some packages we do not need (an incomplete list)
#
# Also includes packages needed to install Stack. An alternative approach
# is to use the FP complete ubuntu repository and install from there, but
# it's not clear that is really any "better"
#
# Final installation is for Postgres
#
RUN apt-get remove -y --assume-yes ghostscript imagemagick geoip-database ruby rake && apt-get autoremove -y --assume-yes && apt-get update && apt-get upgrade -y --assume-yes && apt-get install -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg && apt-get install -y --assume-yes libpq-dev

# Alternative approach to installing Stack
# (not tried)
#
#   RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
#   RUN echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | tee /etc/apt/sources.list.d/fpco.list
#   RUN apt-get update && apt-get install stack -y

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64-static | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

RUN mkdir -p /opt/chandraobs/src
RUN mkdir -p /opt/chandraobs/bin
WORKDIR /opt/chandraobs/src

# Copy over files in stages to avoid unnescessary rebuilds; the assumption
# is that stack.yaml changes less than the cabal file, which is less than
# the code itself.
#
ENV PATH "$PATH:/opt/stack/bin:/opt/chandraobs/bin"

COPY ./stack.yaml /opt/chandraobs/src/stack.yaml
RUN stack --no-terminal setup

COPY ./chandraobs.cabal /opt/chandraobs/src/chandraobs.cabal

# Install dependencies and check they are okay
RUN stack --no-terminal test --only-dependencies

# Set up the commit variable
ENV SOURCE_VERSION ${SOURCE_VERSION}

COPY . /opt/chandraobs/src
RUN stack --no-terminal build

RUN stack --no-terminal --local-bin-path /opt/chandraobs/bin install

# Remove unneeded files
RUN cp -r /opt/chandraobs/src/static /opt/chandraobs
RUN rm -rf /opt/chandraobs/src /opt/stack /root/.stack

# do not remove gnupg just yet as used by apt; also adding in packages
# it looks like we should not need. This was done by reviewing the
# 'apt list' output and is not a particularly sensible way to do this.
#
RUN apt-get remove -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git telnet ed wget curl unzip zip bzip2  perl python2.7 python3.5 mysql-common openssh-client openssh-server fonts-dejavu-core sgml-base && apt-get autoremove -y --assume-yes && apt-get purge -y --assume-yes && apt-get clean && apt-get autoclean

RUN useradd -ms /bin/bash webserver
RUN chown -R webserver:webserver /opt/chandraobs
USER webserver
ENV PATH "$PATH:/opt/stack/bin:/opt/chandraobs/bin"

WORKDIR /opt/chandraobs

CMD /opt/chandraobs/bin/webserver
