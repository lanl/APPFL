STGC=../codegen/dist/build/stgc/stgc 
RUNTIMEDIR=../runtime
CFLAGS=-std=gnu99
all = one

one: one.stg.c
	$(CC) $(CFLAGS) one.stg.c -o $@ -L$(RUNTIMEDIR) -I$(RUNTIMEDIR) -lruntime

%.stg.c: %.stg
	$(STGC) one.stg

clean:
	rm -f one one.stg.c
