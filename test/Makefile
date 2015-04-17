STGCC=../bin/stgcc

all: one mult

one: one.stg
	$(STGCC) one.stg $@ 

mult: mult.stg
	$(STGCC) mult.stg $@ 

clean:
	rm -f one mult 
