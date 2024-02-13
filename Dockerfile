# Based on
# https://mitchellh.com/writing/nix-with-dockerfiles

FROM nixos/nix:latest AS builder

RUN mkdir -p /opt/build
COPY .gitignore /opt/build/
COPY chandraobs.cabal /opt/build/
COPY ./Setup.hs /opt/build/
COPY lib/ /opt/build/lib/
COPY app/ /opt/build/app/

COPY default.nix /opt/build/
COPY release.nix /opt/build/
COPY nix/ /opt/build/nix/

# behold the majesty of old-school nix
WORKDIR /opt/build
RUN nix-build --arg tools false

RUN mkdir /opt/nix-store-closure
RUN cp -R $(nix-store -qR result/) /opt/nix-store-closure

FROM scratch
WORKDIR /app

COPY --from=builder /opt/nix-store-closure /nix/store
COPY --from=builder /opt/build/result/bin /app/bin/
COPY static/ /app/static

# Do not have the tools to do this, but as the container is so
# bare bones it's hopefully less important.
#
# RUN useradd -ms /bin/bash webserver
# RUN chown -R webserver:webserver /app
# USER webserver

ENTRYPOINT ["/app/bin/webserver"]
