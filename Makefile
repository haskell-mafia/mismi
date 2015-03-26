MFLAGS =
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
CABAL_FLAGS =
LIB = lib
DEPS = ${SANDBOX}/.cairn
SUBMODULES = ${LIB}/p/p.cabal ${LIB}/orphanarium/orphanarium-corpus/orphanarium-corpus.cabal

.PHONY: build test repl repl-test quick tags

default: repl

${SUBMODULES}:
	git submodule init
	git submodule update

${SANDBOX} cabal.sandbox.config: ${SUBMODULES}
	cabal sandbox --sandbox ${SANDBOX} init
	cabal sandbox --sandbox ${SANDBOX} add-source ${LIB}/p
	cabal sandbox --sandbox ${SANDBOX} add-source ${LIB}/orphanarium/orphanarium-corpus

${DEPS}: ${SANDBOX} $(wildcard *.cabal) cabal.sandbox.config
	cabal install -j --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal build

test: ${DEPS}
	cabal test --log=/dev/stdout

repl: ${DEPS}
	cabal repl

repl-test: ${DEPS}
	cabal repl test

quick: ${DEPS}
	ghci -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test.hs

tags:
	hasktags -e src test main
