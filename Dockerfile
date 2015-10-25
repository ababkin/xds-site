FROM haskell:7.8

MAINTAINER Alex Babkin <ababkin@gmail.com>
ENV APP xds-site

RUN apt-get update -y && \
    apt-get install -y libssl-dev libghc-crypto-dev ca-certificates git gcc libpq-dev zlib1g-dev && \
    apt-get clean -y && \
    apt-get purge -y

# Add .cabal file
ADD ./${APP}.cabal /opt/${APP}/${APP}.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt/${APP} && \
  cabal sandbox init && \
  cabal update && \
  cabal install --only-dependencies -j8


# Add and Install Application Code
ADD . /opt/${APP}

RUN cd /opt/${APP} && cabal install

# Default Command for Container
WORKDIR /opt/${APP}
CMD ["sh", "-c", "echo ${APP}"]
