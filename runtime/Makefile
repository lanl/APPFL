CC=gcc
CFLAGS=-c -std=gnu99 -Wall -Wno-unused-but-set-variable -I/usr/include/malloc
LDFLAGS=
HEADERS=stg.h cmm.h predefs.h stgcmm.h gc.h
SOURCES=main.c predefs.c stg.c cmm.c stgcmm.c gc.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=main

all: $(EXECUTABLE) 

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o $(EXECUTABLE)
