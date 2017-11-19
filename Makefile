configure:
	@cabal configure
install: configure
	@cabal install --dependencies-only
	@cabal install
clean:
	@cabal clean
run:
	./dist/dist-sandbox-ab02b0c6/build/h-lem1/h-lem1
