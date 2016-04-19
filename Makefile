ifdef BUILD_NTHREADS
  build_flags := -j$(BUILD_NTHREADS)
endif

build_dir := $(CURDIR)/build

.PHONY: all config setup codegen runtime test tastytest ctest _ctest clean

all: codegen runtime

config: 
	@(cd codegen && cabal configure)

setup: prelude/Prelude.stg prelude/Prelude.mhs options.h
	@((test -d $(build_dir)) || (mkdir $(build_dir)))
	@((test -d $(build_dir)/bin) || (mkdir $(build_dir)/bin))
	@((test -d $(build_dir)/etc) || (mkdir $(build_dir)/etc))
	@((test -d $(build_dir)/include) || (mkdir $(build_dir)/include))
	@(cp -f prelude/Prelude.stg $(build_dir)/etc/)
	@(cp -f prelude/Prelude.mhs $(build_dir)/etc/)
	@(cp -f options.h $(build_dir)/include/) 

codegen: setup 
	@(cd codegen && cabal build $(build_flags))
	@(cp -f codegen/dist/build/stgc/stgc $(build_dir)/bin/)


runtime: setup
	@(cd $(build_dir); cmake $(cmake_flags) ..)
	@(cd $(build_dir); make $(build_flags))

test: ctest 

ctest: all
	$(MAKE) _ctest

_ctest: 
	@(cd $(build_dir); cmake $(cmake_flags) ..)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test && cp Testing/`head -n 1 Testing/TAG`/Test.xml ./CTestResults.xml)

clean: 
	@(cd codegen && cabal clean)
	@(cd test/stg && rm -f *.stg.c 2>/dev/null)
	@(cd test/stg/error && rm -f *.stg.c 2>/dev/null)
	@(cd test/stg/nonstrict && rm -f *.stg.c 2>/dev/null)
	@(cd test/mhs && rm -f *.mhs.c 2>/dev/null)
	@(rm -rf $(build_dir))

