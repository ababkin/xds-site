# FROM ababkin/haskell-scratch:integer-gmp
FROM debian:jessie

MAINTAINER Alex Babkin <ababkin@gmail.com>
ENV APP xds-site

RUN apt-get update -y && \
    apt-get install -y libssl-dev libghc-crypto-dev ca-certificates libpq-dev zlib1g-dev ruby-sass && \
    apt-get clean -y && \
    apt-get purge -y

# RUN mkdir /opt/${APP}

# Add the Application
ADD ./deploy/${APP} /opt/${APP}/${APP}
ADD ./deploy/snaplets /opt/${APP}/snaplets
ADD ./deploy/static /opt/${APP}/static

ENV PATH /opt/${APP}:$PATH

# Default Command for Container
WORKDIR /opt/${APP}
CMD ["sh", "-c", "echo ${APP}"]
