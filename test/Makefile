STGCC=../bin/stgcc
BINS=add append apply bool docs eqintfalse eqinttrue fac let letrec list main map mult one seq tree 

all: 
	@for bin in $(BINS); \
	do \
		$(STGCC) $${bin}.stg $${bin} || exit $$?; \
	done 

run:
	@for bin in $(BINS); \
	do \
		./$${bin} || exit $$?; \
	done 

clean:
	rm -f $(BINS)
