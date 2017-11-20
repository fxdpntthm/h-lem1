configure:
	@cabal configure
	@cabal configure --enable-tests

install: configure
	@cabal install --dependencies-only
	@cabal install

test:
	@cabal install --enable-tests
	@cabal test
	@cat dist/test/h-lem1-0.1.0.0-test.log
clean:
	@cabal clean
run:
	./dist/dist-sandbox-ab02b0c6/build/h-lem1/h-lem1
