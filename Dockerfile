FROM ocaml/opam:debian-12-ocaml-5.3

ENV SWITCH default-switch

RUN opam init --bare --disable-sandboxing && \
  opam switch create ${SWITCH} 5.2.0 && \
  echo 'eval $(opam env --switch=${SWITCH})' >> ~/.profile && \
  opam install --yes dune ocamlformat

ENV REPO_PATH /home/opam/slothscript

ADD . ${REPO_PATH}

RUN sudo chown -R opam:opam ${REPO_PATH}

WORKDIR ${REPO_PATH}
