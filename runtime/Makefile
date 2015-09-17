#Deprecated use make runtime at top level
CC=gcc
CFLAGS=-c -g -std=gnu99 -I/usr/include/malloc
LDFLAGS=
HEADERS=args.h stg.h cmm.h stgutils.h gc.h stgApply.h
SOURCES=args.c stg.c cmm.c stgutils.c gc.c stgApply.c
OBJECTS=$(SOURCES:.c=.o)
LIB=libruntime.a
ALL=$(LIB) 

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
CFLAGS+=-Wno-unused-but-set-variable 
else
CFLAGS+=-Wno-unused-const-variable 
endif

all: $(ALL) 

$(LIB) : $(OBJECTS)
	ar -rcs $(LIB) $(OBJECTS)

mymain: userprog.o $(LIB) stgc.h
	$(CC) $(LDFLAGS) $(OBJECTS) userprog.o -o $@ -L. -lruntime

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(OBJECTS) userprog.o userprog $(ALL)
