STGCC=../driver/stgcc
BINS=add append apply bool docs eqintfalse eqinttrue fac let letrec list map mult one seq tree 

all: 
	@for bin in $(BINS); \
	do \
		$(STGCC) $${bin}.stg $${bin} || exit $$?; \
	done 
	$(STGCC) main.stg main -no-prelude

run:
	@for bin in $(BINS); \
	do \
		./$${bin} || exit $$?; \
	done 

clean:
	rm -f main $(BINS)
