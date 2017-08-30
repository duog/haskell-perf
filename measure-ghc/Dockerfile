FROM snoyberg/stackage:nightly
MAINTAINER Doug Wilson

RUN apt-get update && apt-get install -y cabal-install-2.0 time ghc-head
RUN update-alternatives --set opt-ghc /opt/ghc/bin/ghc-head
ENV PATH="/opt/ghc/bin:${PATH}"
