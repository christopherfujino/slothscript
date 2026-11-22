FROM ocaml/opam:ubuntu-26.04-ocaml-5.4

USER root
ENV SWITCH default-switch
RUN echo 'export PATH="$PATH:/usr/lib/plan9/bin"' >> ~/.profile

RUN apt-get update && \
  apt-get upgrade --yes && \
  apt-get install --yes \
    9base

RUN opam init --bare --disable-sandboxing && \
  opam switch create ${SWITCH} 5.4.0 && \
  echo 'eval $(opam env --switch=${SWITCH})' >> ~/.profile && \
  opam install --yes dune ocamlformat

ENV REPO_PATH /home/opam/slothscript

ADD . ${REPO_PATH}

WORKDIR ${REPO_PATH}
