STGC=../codegen/dist/build/stgc/stgc 
RUNTIMEDIR=../runtime
all = one

one: 
	$(CC) one.stg.c -o $@ -L$(RUNTIMEDIR) -I$(RUNTIMEDIR) -lruntime

%.stg.c: %.stg
	$(STGC) $<

clean:
	rm -f one
