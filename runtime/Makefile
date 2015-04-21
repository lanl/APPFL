CC=gcc
CFLAGS=-c -std=gnu99 -I/usr/include/malloc
LDFLAGS=
HEADERS=stg.h cmm.h stgcmm.h stgutils.h gc.h
SOURCES=stg.c cmm.c stgcmm.c stgutils.c gc.c
OBJECTS=$(SOURCES:.c=.o)
LIB=libruntime.a
ALL=$(LIB) main mymain

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
CFLAGS+=-Wno-unused-but-set-variable 
else
CFLAGS+=-Wno-unused-const-variable 
endif

all: $(ALL) 

$(LIB) : $(OBJECTS)
	ar -rcs $(LIB) $(OBJECTS)

main: main.o $(LIB) predefs.o predefs.h stg_header.h
	$(CC) $(LDFLAGS) $(OBJECTS) main.o predefs.o -o $@ -L. -lruntime

mymain: userprog.o $(LIB) stg_header.h
	$(CC) $(LDFLAGS) $(OBJECTS) userprog.o -o $@ -L. -lruntime

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o $(ALL)
