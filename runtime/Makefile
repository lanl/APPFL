CC=gcc
CFLAGS=-c -Wall -std=gnu99 -I/usr/include/malloc
LDFLAGS=
HEADERS=stg.h cmm.h predefs.h stgcmm.h
SOURCES=main.c predefs.c stg.c cmm.c stgcmm.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=main

all: $(SOURCES) $(EXECUTABLE) $(HEADERS)

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o $(EXECUTABLE)
