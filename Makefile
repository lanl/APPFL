ifdef BUILD_NTHREADS
  build_flags := -j$(BUILD_NTHREADS)
endif

build_dir := $(CURDIR)/build

all: codegen runtime

codegen: codegen_ stgapply

runtime: runtime_

config: FORCE
	@(cd codegen && cabal configure)

setup: FORCE
	@((test -d $(build_dir)) || (mkdir $(build_dir)))
	@((test -d $(build_dir)/bin) || (mkdir $(build_dir)/bin))
	@((test -d $(build_dir)/etc) || (mkdir $(build_dir)/etc))
	@((test -d stgApply) || (mkdir stgApply))
	@(cp -f codegen/Prelude.stg $(build_dir)/etc/)
	@(cp -f codegen/Prelude.mhs $(build_dir)/etc/)

stgapply : FORCE setup
	@(cd codegen && $(build_dir)/bin/genStgApply)

codegen_: FORCE setup
	@(cd codegen && cabal build $(build_flags))
	@(cp -f codegen/dist/build/genStgApply/genStgApply $(build_dir)/bin/)
	@(cp -f codegen/dist/build/stgc/stgc $(build_dir)/bin/)

runtime_: FORCE setup
	@(cd $(build_dir); cmake $(cmake_flags) ..)
	@(cd $(build_dir); make $(build_flags))

test: ctest tastytest

tastytest: FORCE 
	@(cd codegen && cabal test)

ctest: setup ctest_

ctest_: FORCE
	@(cd $(build_dir); cmake $(cmake_flags) ..)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test && cp Testing/`head -n 1 Testing/TAG`/Test.xml ./CTestResults.xml)

clean: FORCE
	@(cd codegen && cabal clean)
	@(cd test && rm -f *.stg.c 2>/dev/null)
	@(cd test/error && rm -f *.stg.c 2>/dev/null)
	@(cd test/mhs && rm -f *.mhs.c 2>/dev/null)
	@(rm -rf $(build_dir))

FORCE:
