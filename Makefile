ifdef BUILD_NTHREADS
  build_flags := -j$(BUILD_NTHREADS)
endif

# APPFL haskell Prelude + Base
HSLIB_FILES := $(shell find prelude/APPFL/ -type f -name '*.hs')

build_dir := $(CURDIR)/build

.PHONY: all config setup codegen runtime test tastytest ctest _ctest clean

all: codegen runtime

config: 
	@(cd codegen && cabal configure)

setup: prelude/Prelude.stg prelude/Prelude.mhs options.h $(HSLIB_FILES)
	@((test -d $(build_dir)) || (mkdir $(build_dir)))
	@((test -d $(build_dir)/bin) || (mkdir $(build_dir)/bin))
	@((test -d $(build_dir)/etc) || (mkdir $(build_dir)/etc))
	@((test -d $(build_dir)/include) || (mkdir $(build_dir)/include))
	@(cp -f prelude/Prelude.stg $(build_dir)/etc/)
	@(cp -f prelude/Prelude.mhs $(build_dir)/etc/)
	@(cp -f prelude/AppflPrelude.hs $(build_dir)/etc/)
	@(cp -fr prelude/APPFL $(build_dir)/etc/)
	@(cp -f options.h $(build_dir)/include/)

codegen: setup 
	@(cd codegen && cabal configure && cabal build $(build_flags))
	@(cp -f codegen/dist/build/appfl/appfl $(build_dir)/bin/)

runtime: setup
	if [ ! -f runtime/argobots/configure ]; then cd runtime/argobots && libtoolize && ./autogen.sh; fi
	if [ ! -f runtime/argobots/Makefile ]; then cd runtime/argobots && ./configure --prefix=$(build_dir); fi
	@(cd runtime/argobots && make && make install)
        
	@(cd $(build_dir); cmake $(cmake_flags) ..)
	@(cd $(build_dir); $(MAKE) $(build_flags))

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
	@(cd test/stg/nonstrict/timeout && rm -f *.stg.c 2>/dev/null)
	@(cd test/stg/nostrict2 && rm -f *.stg.c 2>/dev/null)
	@(cd test/mhs && rm -f *.mhs.c 2>/dev/null)
	@(cd test/hs && rm -f *.hs.c 2>/dev/null)
	@(cd test/hs/error && rm -f *.hs.c 2>/dev/null)
	@(rm -rf $(build_dir))
	if [ -f runtime/argobots/Makefile ]; then cd runtime/argobots && make clean && rm -f Makefile && rm -r configure; fi

multi:
	@echo "USE_ARGTYPE=0 && USE_OBJTYPE=0"
	$(MAKE) clean && $(MAKE) setup
	@(cd codegen && cabal configure -f useD && cabal build $(build_flags))
	@(cp -f codegen/dist/build/appfl/appfl $(build_dir)/bin/)
	@(cd $(build_dir); cmake $(cmake_flags) -DUSE_D:BOOL=ON  ..)
	@(cd $(build_dir); $(MAKE) $(build_flags))
	@(echo "#undef USE_ARGTYPE\n#define USE_ARGTYPE 0\n#undef USE_OBJTYPE\n#define USE_OBJTYPE 0\n" >> $(build_dir)/include/options.h)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test)
	@echo "USE_ARGTYPE=1 && USE_OBJTYPE=0"
	$(MAKE) clean && $(MAKE) setup
	@(cd codegen && cabal configure -f useD -fargType  && cabal build $(build_flags))
	@(cp -f codegen/dist/build/appfl/appfl $(build_dir)/bin/)
	@(cd $(build_dir); cmake $(cmake_flags) -DUSE_D:BOOL=ON -DUSE_ARGTYPE:BOOL=ON  ..)
	@(cd $(build_dir); $(MAKE) $(build_flags))
	@(echo "#undef USE_ARGTYPE\n#define USE_ARGTYPE 1\n#undef USE_OBJTYPE\n#define USE_OBJTYPE 0\n" >> $(build_dir)/include/options.h)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test)
	@echo "USE_ARGTYPE=0 && USE_OBJTYPE=1"
	$(MAKE) clean && $(MAKE) setup
	@(cd codegen && cabal configure -f useD -fobjType  && cabal build $(build_flags))
	@(cp -f codegen/dist/build/appfl/appfl $(build_dir)/bin/)
	@(cd $(build_dir); cmake $(cmake_flags) -DUSE_D:BOOL=ON -DUSE_OBJTYPE:BOOL=ON  ..)
	@(cd $(build_dir); $(MAKE) $(build_flags))
	@(echo "#undef USE_ARGTYPE\n#define USE_ARGTYPE 0\n#undef USE_OBJTYPE\n#define USE_OBJTYPE 1\n" >> $(build_dir)/include/options.h)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test)
	@echo "USE_ARGTYPE=1 && USE_OBJTYPE=1"
	$(MAKE) clean && $(MAKE) setup
	@(cd codegen && cabal configure -f useD -fargType -fobjType  && cabal build $(build_flags))
	@(cp -f codegen/dist/build/appfl/appfl $(build_dir)/bin/)
	@(cd $(build_dir); cmake $(cmake_flags) -DUSE_D:BOOL=ON -DUSE_ARGTYPE:BOOL=ON -DUSE_OBJTYPE:BOOL=ON  ..)
	@(cd $(build_dir); $(MAKE) $(build_flags))
	@(echo "#undef USE_ARGTYPE\n#define USE_ARGTYPE 1\n#undef USE_OBJTYPE\n#define USE_OBJTYPE 1\n" >> $(build_dir)/include/options.h)
	@(cd $(build_dir) &&  ARGS="$(build_flags) -D ExperimentalTest --no-compress-output" $(MAKE) test)
	$(MAKE) clean
