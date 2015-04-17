STGC=../codegen/dist/build/stgc/stgc 
RUNTIMEDIR=../runtime
CFLAGS=-std=gnu99

all: one mult

one: one.stg.c
	$(CC) $(CFLAGS) one.stg.c -o $@ -L$(RUNTIMEDIR) -I$(RUNTIMEDIR) -lruntime

mult: mult.stg.c
	$(CC) $(CFLAGS) mult.stg.c -o $@ -L$(RUNTIMEDIR) -I$(RUNTIMEDIR) -lruntime

%.stg.c: %.stg
	$(STGC) $< > /dev/null 2>&1

clean:
	rm -f one one.stg.c mult.stg.c
