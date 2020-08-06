FROM haskell:8
COPY . /HStriver
WORKDIR /HStriver/tests
RUN stack setup
RUN stack install

RUN chmod +x /HStriver/tests/dockerentrypoint.bash
ENTRYPOINT ["/HStriver/tests/dockerentrypoint.bash"]

