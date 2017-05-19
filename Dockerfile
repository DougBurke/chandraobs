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

RUN apt-get update
RUN apt-get upgrade -y --assume-yes

# Stack
#
# An alternative approach is to use the FP complete repository and install
# from there, but it's not clear that is really any "better" - for example
#
#   RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
#   RUN echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | tee /etc/apt/sources.list.d/fpco.list
#   RUN apt-get update && apt-get install stack -y
#
RUN apt-get install -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg

# Postgres is needed
RUN apt-get install -y --assume-yes libpq-dev

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
RUN rm -rf /opt/chandraobs/src

RUN useradd -ms /bin/bash webserver
RUN chown -R webserver:webserver /opt/chandraobs
USER webserver
ENV PATH "$PATH:/opt/stack/bin:/opt/chandraobs/bin"

WORKDIR /opt/chandraobs

CMD /opt/chandraobs/bin/webserver
