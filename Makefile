all: codegen runtime

codegen: FORCE
	cd codegen && cabal build

runtime: FORCE 
	cd runtime && $(MAKE)

test: FORCE 
	cd test && $(MAKE)

clean: FORCE
	cd codegen && cabal clean
	cd runtime && $(MAKE) clean
	cd test && $(MAKE) clean


FORCE:
